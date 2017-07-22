
// 1. lex + tree
// 2. tree shape check
// 3. syntax basics
// 4. syntax 

#include "Pch.h"
#include "SourceManager.h"
#include "Syntax.h"
#include "Reflect.h"

namespace Ast {
    struct Node;
    struct Type;
    struct FunctionCall;
    struct FunctionDecl;
    struct Number;

    struct Node : Reflect::AbstractReflected {
        REFLECT_DECL();
        virtual void type_check() {
            assert(0);
        }

        Type* m_type{ nullptr };
    };
    REFLECT_BEGIN(Node)
        REFLECT_FIELD(m_type)
    REFLECT_END()


    struct Type : Node {
        REFLECT_DECL();
        Type(const std::string& s);
        std::string m_name;
    };
    REFLECT_BEGIN(Type)
        //REFLECT_PARENT(Node)
        REFLECT_FIELD(m_name)
    REFLECT_END()

    static Ast::Type s_typeType("Type");
    static Ast::Type s_typeInt("int");
    static Ast::Type s_typeDouble("double");
    static Ast::Type s_typeVoid("void");
    static Ast::Type s_typeF_double_double("double(*)(double)");

    Type::Type(const std::string& s) : m_name(s) {
        m_type = &s_typeType;
    }

    struct Decl : Node {
        REFLECT_DECL();
    };

    REFLECT_BEGIN(Decl)
        REFLECT_PARENT(Node)
    REFLECT_END()

    struct Number : Node {
        REFLECT_DECL();
        Number( Syntax::Number* n) : m_num(n) {}
        Syntax::Number* m_num;

        void type_check() {
            m_type = &s_typeDouble;
        }
        static std::string toString(const void* p) {
            auto n = static_cast<const Number*>(p);
            return n->m_num->text();
        }
    };
    REFLECT_BEGIN(Number)
        REFLECT_PARENT(Node)
        REFLECT_TO_STRING(Number::toString)
    REFLECT_END()

    struct Module : Node {
        REFLECT_DECL();
        void type_check() override {
            m_type = &s_typeVoid;
        }
        std::vector<Node*> m_items;
    };

    REFLECT_BEGIN(Module)
        REFLECT_PARENT(Node)
        REFLECT_FIELD(m_items)
    REFLECT_END()

    struct FunctionCall : public Node {
        REFLECT_DECL();
        const Node* m_func{ nullptr };
        std::vector< Node* > m_args;
        FunctionCall( const Node* func, std::vector< Node* >&& args )
            : m_func(func), m_args(args) {
        }

        void type_check() {
            if (m_func->m_type) {
                m_type = &s_typeDouble;//TODO
            }
        }
    };

    REFLECT_BEGIN(FunctionCall)
        REFLECT_PARENT(Node)
        //REFLECT_FIELD(m_func)
        REFLECT_FIELD(m_args)
    REFLECT_END()

    struct Symbol : public Node {
        REFLECT_DECL();

        Symbol(std::string&& n, Syntax::Symbol* s) : m_name(n), m_sym(s) {
        }

        void type_check() {
        }

        std::string m_name;
        Syntax::Symbol* m_sym;

        std::string text() const {
            return m_sym->text();
        }

        static std::string toString(const void* obj) {
            auto sym = static_cast<const Symbol*>(obj);
            return string_format("%s\"}", sym->dynamicType()->name, sym->m_name.c_str());
        }
    };

    REFLECT_BEGIN(Symbol)
        REFLECT_PARENT(Node)
        //REFLECT_TO_STRING(Symbol::toString)
        REFLECT_FIELD(m_name)
        //REFLECT_FIELD(m_sym)
    REFLECT_END()

    struct FunctionDecl : public Node {
        REFLECT_DECL();

        std::vector< Symbol* > m_arg_syms;
        Node* m_body = nullptr;

        FunctionDecl() {}
        FunctionDecl( std::vector<Symbol*>& args, Node* body ) {
            m_arg_syms.swap( args );
            m_body = body;
        }
        void type_check() {
            if (m_body->m_type == nullptr) return;
            if (any_of(m_arg_syms, [](Symbol*s) { return s->m_type == nullptr; })) { return; }
            m_type = &s_typeF_double_double;//TODO
        }
    };

    REFLECT_BEGIN(FunctionDecl)
        REFLECT_PARENT(Node)
        REFLECT_FIELD(m_arg_syms)
        REFLECT_FIELD(m_body)
    REFLECT_END()


    struct Sequence : public Node {
        REFLECT_DECL();
        std::vector<Node*> m_items;
        void type_check() {
            if (m_items.size() == 0 ) {
                m_type = &s_typeVoid;
            }
            else if (auto t = m_items.back()->m_type) {
                m_type = t;
            }
        }
    };

    REFLECT_BEGIN(Sequence)
        REFLECT_PARENT(Node)
        REFLECT_FIELD(m_items)
    REFLECT_END()

    struct Argument : public Node {
        REFLECT_DECL();
        Argument(Symbol* s) : m_sym(s) {
        }
        Symbol* m_sym;

        void type_check() {
            if (m_sym->m_type) {
                m_type = m_sym->m_type;
            }
        }
    };

    REFLECT_BEGIN(Argument)
        REFLECT_PARENT(Node)
        REFLECT_FIELD(m_sym)
    REFLECT_END()

    struct Reference : public Node {
        REFLECT_DECL();
        Reference(Node* s) : m_sym(s) {
        }
        Node* m_sym;
    };

    REFLECT_BEGIN(Reference)
        REFLECT_PARENT(Node)
        //REFLECT_FIELD(m_sym)
    REFLECT_END()

    struct Scope : public Node {
        REFLECT_DECL();
        Scope(Node* c) : m_child(c) {}
        Node* m_child{ nullptr };

        void type_check() {
            if (m_child->m_type) {
                m_type = m_child->m_type;
            }
        }
    };

    REFLECT_BEGIN(Scope)
        REFLECT_PARENT(Node)
        REFLECT_FIELD(m_child)
    REFLECT_END()


    struct Definition : public Node {
        REFLECT_DECL();

        Symbol* m_sym = nullptr;
        Node* m_value = nullptr;

        Definition() {}
        Definition( Symbol* sym, Node* value ) : m_sym( sym ), m_value( value ) {}

        virtual void type_check() {
            if (auto t = m_value->m_type) {
                m_type = t;
                if (m_sym->m_type == nullptr) {
                    m_sym->m_type = t;
                }
                m_type = &s_typeVoid;
            }
        }
    };

    REFLECT_BEGIN(Definition)
        REFLECT_PARENT(Node)
        REFLECT_FIELD(m_sym)
		REFLECT_FIELD(m_value)
    REFLECT_END()

    void collect_nodes(std::vector<Ast::Node*>& out, Reflect::Var var) {
        switch (var.type->kind) {
            case Reflect::Kind::Pointer: {
                if (var.type->sub->extends<Node>()) {
                    auto p = *static_cast<Node**>(var.addr);
                    if (p) {
                        out.push_back(p);
                        collect_nodes(out, p);
                    }
                }
                break;
            }
            case Reflect::Kind::Array: {
                auto a = static_cast<std::vector<char>*>(var.addr);
                size_t esize = var.type->sub->size;
                size_t bsize = a->size();
                assert(bsize % esize == 0);
                size_t count = bsize / esize;
                for (int i = 0; i < count; ++i) {
                    collect_nodes(out, Reflect::Var(&a->at(0) + i * esize, var.type->sub));
                }
                break;
            }
            case Reflect::Kind::Record: {
                for (const Reflect::Type* c = var.type; c; c = c->parent) {
                    for (auto& f : c->fields) {
                        collect_nodes(out, var[f]);
                    }
                }
                break;
            }
            case Reflect::Kind::String:
            case Reflect::Kind::Void:
                break;
            default:
                assert(0);
        }
    }
        
    bool type_check(Ast::Node* top_node) {
        std::vector<Ast::Node*> todo{ top_node };
        collect_nodes(todo, top_node);
        // very dumb, just iterate
        while (todo.size()) {
            for (auto n : todo) {
                n->type_check();
            }
            auto pre = todo.size();
            erase_if(todo, [](Ast::Node* n) { return n->m_type != nullptr;  });
            if (todo.size() == pre) {
                int x; x = 0;
            }
        }
        return true;
    }
}

namespace Parse {

    struct State;
    typedef Iter<Syntax::Atom*> Args;

    struct Parser {
        virtual ~Parser() {}
        virtual Ast::Node* parse(State* state, Args& args) const = 0;
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

        void addParser(const std::string& sym, Parser* value) {
            auto p = syms.back().insert_or_assign(sym, Pair{ value, nullptr } );
            assert(p.second);
        }

        void addSym(const std::string& sym, Ast::Node* node) {
            auto p = syms.back().insert_or_assign(sym, Pair{ nullptr, node });
            assert(p.second);
        }

        Ast::Symbol* symbol(Syntax::Atom* atom) {
            if (auto sym = dynamic_cast<Syntax::Symbol*>(atom)) {
                auto ret = new Ast::Symbol(sym->text(), sym);
                if (sym->m_type) {
                    auto n = this->parse(sym->m_type);
                    auto t = dynamic_cast<Ast::Type*>(n);
                    assert(t);
                    ret->m_type = t;
                }
                return ret;
            }
            assert(0);
            return nullptr;
        }

        Ast::Node* parse(Syntax::Atom* atom) {
            if (auto sym = dynamic_cast<Syntax::Symbol*>(atom)) {
                auto p = lookup(sym->text());
                verify(p.first == nullptr);
                return reference(p.second);
            }
            else if (auto list = dynamic_cast<Syntax::List*>(atom)) {
                assert(list);
                assert(list->items.size());
                auto& items = list->items;
                auto sym = symbol(items[0]);
                Args args{ items }; args.advance();
                auto p = lookup(sym->text());
                if (p.first) {
                    return p.first->parse(this, args);
                }
                else {
                    std::vector<Ast::Node*> fa;
                    for (auto a : args) {
                        fa.push_back( parse(a) );
                    }
                    return new Ast::FunctionCall(reference(p.second), std::move(fa));
                }
            }
            else if (auto num = dynamic_cast<Syntax::Number*>(atom)) {
                return new Ast::Number(num);
            }
            verify(0);
            return nullptr;
        }

    protected:

        typedef std::pair<Parser*, Ast::Node*> Pair;
        Ast::Node* reference(Ast::Node* n) {
            //return new Ast::Reference(n);
            return n;
        }

        Pair lookup(const std::string& sym) const {
            for (auto&& cur : reversed(syms)) {
                auto x = cur.find(sym);
                if (x != cur.end()) {
                    return x->second;
                }
            }
            verify(0);
            return Pair{};
        }
        std::list< std::map<std::string, Pair> > syms;
    };

    struct Define : public Parser {
        Ast::Node* parse( State* state, Args& args ) const override {
            auto sym = state->symbol( args.cur() );
            args.advance();
            auto val = state->parse( args.cur() );
            args.advance();
            verify( args.used() );
            auto ret = new Ast::Definition( sym, val );
            state->addSym( sym->text(), ret );
            return ret;
        }
    };

    struct Lambda : public Parser {

        Ast::FunctionDecl* parse( State* state, Args& args ) const override {
            state->enterScope();
            auto ain = dynamic_cast<Syntax::List*>(args.cur());
            args.advance();
            std::vector< Ast::Symbol* > arg_syms;
            for( auto i : ain->items ) {
                arg_syms.push_back( state->symbol( i ) );
            }

            Syntax::Atom* rhs = args.cur();
            args.advance();
            verify( args.used() );

            for( auto a : arg_syms ) {
                state->addSym( a->m_name, new Ast::Argument(a) );
            }
            Ast::Node* body = state->parse( rhs );
            state->leaveScope();

            return new Ast::FunctionDecl( arg_syms, body );
        }
    };

    struct Let : public Parser {
        Ast::Scope* parse( State* state, Args& args ) const override {
            auto lets = dynamic_cast<Syntax::List*>( args.cur() );
            args.advance();
            auto body = args.cur();
            args.advance();
            verify(args.used());

            state->enterScope();
            auto seq = new Ast::Sequence();
            for( auto pair : lets->items) {
                auto cur = dynamic_cast<Syntax::List*>( pair );
                verify(cur->size() == 2);
                auto sym = state->symbol(cur->items[0]);
                verify(sym);
                Ast::Node* val = state->parse(cur->items[1]);
                state->addSym(sym->text(), val);
                seq->m_items.push_back(new Ast::Definition(sym, val));
            }
            seq->m_items.push_back(state->parse(body));
            state->leaveScope();

            return new Ast::Scope(seq);
        }
    };
    
    Ast::Module* module( Syntax::List* syntax ) {
        State state;
        state.addParser( "define", new Define() );
        state.addParser( "lambda", new Lambda() );
        state.addParser( "let", new Let() );
        state.addSym( "int", &Ast::s_typeInt );
        state.addSym( "double", &Ast::s_typeDouble );
        state.addSym( "void", &Ast::s_typeVoid );
        auto module = new Ast::Module();
        for (auto c : syntax->items) {
            Ast::Node* n = state.parse(c);
            module->m_items.push_back(n);
        }
        return module;
    }
}

int main( int argc, const char* argv[] ) {
    if( argc < 2 ) {
        Error.fmt( "Need a script to run" );
        return 1;
    }
    try {
        Syntax::SourceManager smanager;
        Syntax::List* syntax = Syntax::parse_file( smanager, argv[1] );
        verify( syntax );
        Ast::Module* sema = Parse::module( syntax );
        verify( sema );
        Reflect::printVar(sema);
        Ast::type_check(sema);
        Reflect::printVar(sema);
    }
    catch( float ) {
        return 1;
    }
    return 0;
}
