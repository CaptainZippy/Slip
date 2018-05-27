#include "pch/Pch.h"
#include "Slip.h"
#include "Parse.h"
#include "Lex.h"
#include "Ast.h"

namespace Slip::Parse {
    struct State;

    template<typename T>
    struct Iter {
        Iter() = default;

        Iter(std::vector<T>& v) {
            T* t = v.size() ? &v[0] : nullptr;
            m_begin = t;
            m_end = t + v.size();
        }

        Iter(array_view<T> v) {
            m_begin = v.begin();
            m_end = v.end();
        }

        T cur() const {
            assert(size());
            return *m_begin;
        }

        bool advance() {
            assert(m_begin < m_end);
            m_begin += 1;
            return m_begin < m_end;
        }

        bool used() const {
            return m_begin == m_end;
        }

        size_t size() const {
            return m_end - m_begin;
        }

        bool match() {
            return used();
        }
        template<typename F, typename...R>
        bool match(F f, R...r) {
            f = cur();
            if (advance()) {
                return match(r...);
            }
            return false;
        }
        T* begin() { return m_begin; }
        T* end() { return m_end; }

        T* m_begin{ nullptr };
        T* m_end{ nullptr };
    };
    typedef Iter<Lex::Atom*> Args;

    struct Parser {
        virtual ~Parser() {}
        virtual Result parse(State* state, Args& args, Ast::Node** out) const final;
    protected:
        virtual Result _parse(State* state, Args& args, Ast::Node** out) const = 0;
    };

    struct State {

        State() {
            enterScope();
        }

        ~State() {
            leaveScope();
        }

        void enterScope() {
            syms.resize(syms.size() + 1);
        }

        void leaveScope() {
            syms.pop_back();
        }

        void addParser(string_view sym, Parser* value) {
            auto s = istring::make(sym);
            auto p = syms.back().insert_or_assign(s, Pair{ value, nullptr });
            assert(p.second);
        }

        void addSym(string_view sym, Ast::Named* node) {
            auto s = istring::make(sym);
            auto p = syms.back().insert_or_assign(s, Pair{ nullptr, node });
            assert(p.second);
        }

        Lex::Symbol* symbol(Lex::Atom* atom) {
            if (auto sym = dynamic_cast<Lex::Symbol*>(atom)) {
                return sym;
            }
            return nullptr;
        }

        Result _parseType(Lex::Atom* atom, Ast::Type** out) {
            if (atom == nullptr) {
                *out = nullptr;
                return Result::OK;
            }
            else {
                Ast::Node* pt;
                RETURN_IF_FAILED(parse(atom, &pt));
                Ast::Node* et;
                RETURN_IF_FAILED(evaluate(pt, &et));
                auto t = dynamic_cast<Ast::Type*>(et);
                RETURN_RES_IF(Result::ERR, !t);
                *out = t;
                return Result::OK;
            }
            return Result::ERR;
        }


        Result parse(Lex::Atom* atom, Ast::Node** out);

        Result evaluate(Ast::Node* node, Ast::Node** out);

        Result getOrCreateArrayType(Ast::Type* elemType, Ast::Type** out) {
            auto it = m_arrays.find(elemType);
            if (it == m_arrays.end()) {
                auto t = new Ast::Type(string_concat("array_", elemType->m_name));
                t->m_elemType = t;
                it = m_arrays.emplace(elemType, t).first;
            }
            *out = it->second;
            return Result::OK;
        }

        //protected:

        typedef pair<Parser*, Ast::Named*> Pair;
        Ast::Reference* reference(Ast::Named* n) {
            auto r = new Ast::Reference(n);
            if (auto t = n->m_type) {
                r->m_type = t;
            }
            return r;
        }

        Result lookup(string_view sym, const Pair** out) const {
            auto s = istring::make(sym);
            for (auto&& cur : reversed(syms)) {
                auto x = cur.find(s);
                if (x != cur.end()) {
                    *out = &x->second;
                    return Result::OK;
                }
            }
            RETURN_RES_IF(Result::ERR, true, "symbol not found '%s'", s.c_str());
        }
        list< map<istring, Pair> > syms;
        map< Ast::Type*, Ast::Type* > m_arrays;
    };

    struct Evaluator : public State {

        Result dispatch(Ast::Node* node, Ast::Node** out) {
            return Ast::dispatch(node, *this, out);
        }

        Result operator()(Ast::Node* node, Ast::Node** out) {
            return Result::ERR;
        }

        Result operator()(Ast::Reference* node, Ast::Node** out) {
            *out = node->m_target;
            return Result::OK;
        }

        Result operator()(Ast::FunctionCall* node, Ast::Node** out) {
            Ast::Node* funcnode;
            RETURN_IF_FAILED( dispatch(node->m_func, &funcnode) );
            auto func = dynamic_cast<Ast::FunctionDecl*>(funcnode);
            RETURN_RES_IF(Result::ERR, !func);
            Ast::Node* ret;
            vector<Ast::Node*> args;
            for (auto a : node->m_args) { // todo defer arg evaluation to caller
                Ast::Node* e;
                RETURN_IF_FAILED(dispatch(a, &e));
                args.push_back(e);
            }
            RETURN_IF_FAILED(func->invoke(this, args, &ret));
            *out = ret;
            return Result::OK;
        }
    };

    struct Define;
    struct Func;
    struct Let;
    struct If;
    struct Cond;
    struct Begin;
}

using namespace Slip;

#define WITH(...) [&](auto&_){ __VA_ARGS__; }

static Result matchLex(Parse::Args& args) {
    RETURN_RES_IF(Result::ERR, !args.used());
    return Result::OK;
}

template<typename HEAD, typename...REST>
static Result matchLex(Parse::Args& args, HEAD** head, REST**...rest) {
    auto h = dynamic_cast<HEAD*>(args.cur());
    RETURN_RES_IF(Result::ERR, !h);
    args.advance();
    *head = h;
    return matchLex(args, rest...);
}

Result Parse::State::evaluate(Ast::Node* node, Ast::Node** out) {
    *out = nullptr;
    RETURN_IF_FAILED(static_cast<Evaluator*>(this)->dispatch(node, out) );
    return Result::OK;
}


Result Parse::State::parse(Lex::Atom* atom, Ast::Node** out) {
    *out = nullptr;
    if (auto sym = dynamic_cast<Lex::Symbol*>(atom)) {
        const Pair* p;
        RETURN_IF_FAILED(lookup(sym->text(), &p));
        RETURN_RES_IF(Result::ERR, p->first != nullptr);
        *out = reference(p->second);
        return Result::OK;
    }
    else if (auto list = dynamic_cast<Lex::List*>(atom)) {
        RETURN_RES_IF(Result::ERR, !list);
        RETURN_RES_IF(Result::ERR, list->size() == 0);
        auto items = list->items();
        auto sym = symbol(items[0]);
        Args args{ items }; args.advance();
        const Pair* p;
        RETURN_IF_FAILED(lookup(sym->text(), &p));
        if (p->first) { // a builtin
            return p->first->parse(this, args, out);
        }
        else {
            vector<Ast::Node*> fa;
            for (auto a : args) {
                Ast::Node* n;
                RETURN_IF_FAILED(parse(a, &n));
                fa.push_back(n);
            }
            *out = new Ast::FunctionCall(reference(p->second), move(fa));
            return Result::OK;
        }
    }
    else if (auto num = dynamic_cast<Lex::Number*>(atom)) {
        Ast::Type* t;
        RETURN_IF_FAILED(this->_parseType(num->m_decltype, &t));
        auto r = new Ast::Number(num->text(), WITH(_.m_loc=num->m_loc, _.m_type = t));
        *out = r;
        return Result::OK;
    }
    else if (auto str = dynamic_cast<Lex::String*>(atom)) {
        Ast::Type* t;
        RETURN_IF_FAILED(this->_parseType(str->m_decltype, &t));
        auto r = new Ast::String(str->text(), WITH(_.m_loc = str->m_loc, _.m_type = t));
        *out = r;
        return Result::OK;
    }
    RETURN_RES_IF(Result::ERR, true);
}

struct Parse::Define : Parse::Parser {
    Result _parse(State* state, Args& args, Ast::Node** out) const override {
        Lex::Symbol* lname;
        Lex::Atom* lval;
        RETURN_IF_FAILED(matchLex(args, &lname, &lval));
        Ast::Node* nval;
        RETURN_IF_FAILED(state->parse(lval, &nval));

        auto ret = new Ast::Definition(lname->text(), nval, WITH(_.m_loc = lname->m_loc));
        state->addSym(lname->text(), ret);

        *out = ret;
        return Result::OK;
    }
};





struct Parse::Func : Parse::Parser {
    Result _parse(State* state, Args& args, Ast::Node** out) const override {
        *out = nullptr;
        Lex::Symbol* lname;
        Lex::List* largs;
        Lex::Atom* lbody;
        RETURN_IF_FAILED( matchLex(args, &lname, &largs, &lbody) );
        auto func = new Ast::FunctionDecl(lname->text(), WITH(_.m_loc = lname->m_loc));
        state->addSym(func->m_name, func);
        state->enterScope();
        if (auto a = largs->m_decltype) {
            Ast::Type* t;
            RETURN_IF_FAILED(state->_parseType(a, &t));
            auto r = new Ast::Node(WITH(_.m_type = t));
            func->m_returnType = r;
        }
        for (auto item : largs->items()) {
            auto sym = state->symbol(item);
            RETURN_RES_IF(Result::ERR, sym == nullptr);
            Ast::Type* t;
            RETURN_IF_FAILED(state->_parseType(item->m_decltype, &t));
            auto arg = new Ast::Argument(sym->text(), WITH( _.m_loc = sym->m_loc, _.m_type = t ));
            func->m_args.push_back(arg);
        }
        for (auto a : func->m_args) {
            state->addSym(a->m_name, a);
        }
        Ast::Node* body;
        RETURN_IF_FAILED(state->parse(lbody, &body));
        state->leaveScope();
        func->m_body = body;
        *out = func;
        return Result::OK;
    }
};


struct Parse::Let : Parse::Parser {
    Result _parse(State* state, Args& args, Ast::Node** out) const override {
        Lex::List* llets;
        Lex::Atom* lbody;
        RETURN_IF_FAILED(matchLex(args, &llets, &lbody));

        state->enterScope();
        auto seq = new Ast::Sequence();
        for (auto litem : llets->items()) {
            Lex::List* lpair = dynamic_cast<Lex::List*>(litem);
            RETURN_RES_IF(Result::ERR, !lpair);
            Lex::Symbol* lsym;
            Lex::Atom* lval;
            Args tmpargs{lpair->items()};
            RETURN_IF_FAILED(matchLex(tmpargs, &lsym, &lval));
            Ast::Node* aval;
            RETURN_IF_FAILED(state->parse(lval, &aval));
            Ast::Type* t;
            RETURN_IF_FAILED(state->_parseType(lsym->m_decltype, &t));

            auto def = new Ast::Definition(lsym->text(), aval, WITH( _.m_loc = lsym->m_loc, _.m_type = t));
            state->addSym(lsym->text(), def);
            seq->m_items.push_back(def);
        }
        Ast::Node* abody;
        RETURN_IF_FAILED(state->parse(lbody, &abody));
        seq->m_items.push_back(abody);
        state->leaveScope();

        *out = seq;
        return Result::OK;
    }
};

struct Parse::If : Parse::Parser {
    Result _parse(State* state, Args& args, Ast::Node** out) const override {
        Lex::Atom* lcond;
        Lex::Atom* ltrue;
        Lex::Atom* lfalse;
        RETURN_IF_FAILED(matchLex(args, &lcond, &ltrue, &lfalse));

        Ast::Node* ncond;
        Ast::Node* ntrue;
        Ast::Node* nfalse;
        RETURN_IF_FAILED(state->parse(lcond, &ncond));
        RETURN_IF_FAILED(state->parse(ltrue, &ntrue));
        RETURN_IF_FAILED(state->parse(lfalse, &nfalse));

        *out = new Ast::If(ncond, ntrue, nfalse);
        return Result::OK;
    }
};

struct Parse::Cond : Parse::Parser {
    Result _parse(State* state, Args& args, Ast::Node** out) const override {
        *out = nullptr;
        RETURN_RES_IF(Result::ERR, args.size() < 1);
        vector< pair<Ast::Node*, Ast::Node*> > cases;
        for (auto arg : args) {
            auto pair = dynamic_cast<Lex::List*>(arg);
            RETURN_RES_IF(Result::ERR, pair == nullptr);
            RETURN_RES_IF(Result::ERR, pair->size() != 2);

            Ast::Node* cond;
            RETURN_IF_FAILED(state->parse(pair->at(0), &cond));
            Ast::Node* iftrue;
            RETURN_IF_FAILED(state->parse(pair->at(1), &iftrue));
            cases.emplace_back(cond, iftrue);
        }
        auto cond = new Ast::Cond;
        cond->m_cases.swap(cases);
        *out = cond;
        return Result::OK;
    }
};

struct Parse::Begin : Parse::Parser {
    Result _parse(State* state, Args& args, Ast::Node** out) const override {
        *out = nullptr;
        auto ret = new Ast::Sequence;
        for (auto arg : args) {
            Ast::Node* n;
            RETURN_IF_FAILED(state->parse(arg, &n));
            ret->m_items.push_back(n);
        }
        *out = ret;
        return Result::OK;
    }
};

static Result array_intrin(Parse::Evaluator* eval, array_view<Ast::Node*> args, Ast::Node** out) {
    RETURN_RES_IF(Result::ERR, args.size() != 1);
    Ast::Type* et = dynamic_cast<Ast::Type*>(args[0]);
    Ast::Type* at;
    RETURN_IF_FAILED(eval->getOrCreateArrayType(et, &at));
    *out = at;
    return Result::OK;
}

static Result array_size_intrin(Parse::Evaluator* eval, array_view<Ast::Node*> args, Ast::Node** out) {
    *out = nullptr;
    RETURN_RES_IF(Result::ERR, args.size() != 1);
    return Result::ERR;
}

static Result array_at_intrin(Parse::Evaluator* eval, array_view<Ast::Node*> args, Ast::Node** out) {
    *out = nullptr;
    RETURN_RES_IF(Result::ERR, args.size() != 2);
    return Result::ERR;
}



Slip::unique_ptr_del<Ast::Module> Parse::module(Lex::List& lex) {
    State state;
    state.addParser("define", new Define());
    state.addParser("func", new Func());
    state.addParser("if", new If());
    state.addParser("cond", new Cond());
    state.addParser("let", new Let());
    state.addParser("begin", new Begin());
    state.addSym("int", &Ast::s_typeInt);
    state.addSym("double", &Ast::s_typeDouble);
    state.addSym("void", &Ast::s_typeVoid);
    state.addSym("string", &Ast::s_typeString);
    state.addSym("eq_i?", Ast::FunctionDecl::makeBinaryOp("eq", new Ast::Argument("a", &Ast::s_typeInt), new Ast::Argument("b", &Ast::s_typeInt), &Ast::s_typeBool));
    state.addSym("lt_i?", Ast::FunctionDecl::makeBinaryOp("lt", new Ast::Argument("a", &Ast::s_typeInt), new Ast::Argument("b", &Ast::s_typeInt), &Ast::s_typeBool));
    state.addSym("add_i", Ast::FunctionDecl::makeBinaryOp("add", new Ast::Argument("a", &Ast::s_typeInt), new Ast::Argument("b", &Ast::s_typeInt), &Ast::s_typeInt));
    state.addSym("sub_i", Ast::FunctionDecl::makeBinaryOp("sub", new Ast::Argument("a", &Ast::s_typeInt), new Ast::Argument("b", &Ast::s_typeInt), &Ast::s_typeInt));
    state.addSym("puts", Ast::FunctionDecl::makeIntrinsic("prns", nullptr, &Ast::s_typeInt, { new Ast::Argument("s", &Ast::s_typeString) } ));
    state.addSym("puti", Ast::FunctionDecl::makeIntrinsic("prni", nullptr, &Ast::s_typeInt, { new Ast::Argument("s", &Ast::s_typeInt) }));
    state.addSym("array", Ast::FunctionDecl::makeIntrinsic("array", array_intrin, &Ast::s_typeType, { new Ast::Argument("s", &Ast::s_typeType) }));
    {
        Ast::Type* at;
        state.getOrCreateArrayType(&Ast::s_typeString, &at);
        state.addSym("size", Ast::FunctionDecl::makeIntrinsic("array_size", array_size_intrin, &Ast::s_typeInt, { new Ast::Argument("a", at) }));
        state.addSym("at", Ast::FunctionDecl::makeIntrinsic("array_at", array_at_intrin, &Ast::s_typeString,
            { new Ast::Argument("a", at), new Ast::Argument("i", &Ast::s_typeInt) }));
    }

    state.addSym("true", new Ast::Argument("true", &Ast::s_typeBool));
    state.addSym("false", new Ast::Argument("false", &Ast::s_typeBool));

    auto module = make_unique_del<Ast::Module>();
    for (auto c : lex.items()) {
        Ast::Node* n;
        THROW_IF_FAILED(state.parse(c, &n));
        module->m_items.push_back(n);
    }
    return module;
}

Result Parse::Parser::parse(State* state, Args& args, Ast::Node** out) const {
    return  _parse(state, args, out);
}
