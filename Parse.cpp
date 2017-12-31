#include "pch/Pch.h"
#include "Parse.h"

Result Parse::State::parse(Lex::Atom* atom, Ast::Node** out) {
    if (auto sym = dynamic_cast<Lex::Symbol*>(atom)) {
        const Pair* p;
        RETURN_IF_FAILED(lookup(sym->text(), &p));
        RETURN_RES_IF(Result::ERR, p->first != nullptr);
        *out = reference(p->second);
        return Result::OK;
    }
    else if (auto list = dynamic_cast<Lex::List*>(atom)) {
        RETURN_RES_IF(Result::ERR, !list);
        RETURN_RES_IF(Result::ERR, list->items.size()==0);
        auto& items = list->items;
        auto sym = symbol(items[0]);
        Args args{ items }; args.advance();
        const Pair* p;
        RETURN_IF_FAILED(lookup(sym->text(), &p));
        if (p->first) { // a builtin
            return p->first->parse(this, args, out);
        }
        else { 
            std::vector<Ast::Node*> fa;
            for (auto a : args) {
                Ast::Node* n;
                RETURN_IF_FAILED(parse(a, &n));
                fa.push_back(n);
            }
            *out = new Ast::FunctionCall(reference(p->second), std::move(fa));
            return Result::OK;
        }
    }
    else if (auto num = dynamic_cast<Lex::Number*>(atom)) {
        Ast::Type* t;
        RETURN_IF_FAILED(this->_parseType(num->m_decltype, &t));
        auto r = new Ast::Number( num->text() );
        r->m_loc = num->m_loc;
        r->m_type = t;
        *out = r;
        return Result::OK;
    }
    else if (auto str = dynamic_cast<Lex::String*>(atom)) {
        Ast::Type* t;
        RETURN_IF_FAILED(this->_parseType(str->m_decltype, &t));
        auto r = new Ast::String(str->text());
        r->m_loc = str->m_loc;
        r->m_type = t;
        *out = r;
        return Result::OK;
    }
    RETURN_RES_IF(Result::ERR, true);
}

Result Parse::Define::_parse( State* state, Args& args, Ast::Node** out ) const  {
    auto sym = state->symbol( args.cur() );
    args.advance();
    Ast::Node* val;
    RETURN_IF_FAILED(state->parse(args.cur(), &val));
    args.advance();
    RETURN_RES_IF(Result::ERR, args.used()==false);
    auto ret = new Ast::Definition( sym->text(), val );
    ret->m_loc = sym->m_loc;
    state->addSym( sym->text(), ret );
    *out = ret;
    return Result::OK;
}

Result Parse::Func::_parse( State* state, Args& args, Ast::Node** out ) const {
    auto sym = state->symbol(args.cur());
    auto func = new Ast::FunctionDecl(sym->text());
    func->m_loc = sym->m_loc;
    args.advance();
    state->addSym(func->m_name, func);
    state->enterScope();
    auto argsList = dynamic_cast<Lex::List*>(args.cur());
    if (auto a = argsList->m_decltype) {
        Ast::Type* t;
        RETURN_IF_FAILED(state->_parseType(a, &t));
        auto r = new Ast::Node; r->m_type = t;
        func->m_returnType = r;
    }
    for( auto item : argsList->items ) {
        auto sym = state->symbol(item);
        RETURN_RES_IF(Result::ERR, sym == nullptr);
        auto arg = new Ast::Argument(sym->text());
        arg->m_loc = sym->m_loc;
        Ast::Type* t;
        RETURN_IF_FAILED(state->_parseType(item->m_decltype, &t));
        arg->m_type = t;
        func->m_args.push_back( arg );
    }
    args.advance();

    Lex::Atom* rhs = args.cur();
    args.advance();
    RETURN_RES_IF(Result::ERR, args.used()==false );

    for( auto a : func->m_args ) {
        state->addSym( a->m_name, a );
    }
    Ast::Node* body;
    RETURN_IF_FAILED(state->parse( rhs, &body ));
    state->leaveScope();
    func->m_body = body;
    *out = func;
    return Result::OK;
}


 Result Parse::Let::_parse( State* state, Args& args, Ast::Node** out) const {
    auto lets = dynamic_cast<Lex::List*>( args.cur() );
    RETURN_RES_IF(Result::ERR, !lets);
    args.advance();
    auto body = args.cur();
    args.advance();
    RETURN_RES_IF(Result::ERR, args.used()==false);

    state->enterScope();
    auto seq = new Ast::Sequence();
    for( auto pair : lets->items) {
        auto cur = dynamic_cast<Lex::List*>( pair );
        RETURN_RES_IF(Result::ERR, !cur);
        RETURN_RES_IF(Result::ERR, cur->size()!=2);
        auto sym = dynamic_cast<Lex::Symbol*>(cur->items[0]);
        RETURN_RES_IF(Result::ERR, sym);
        Ast::Node* val;
        RETURN_IF_FAILED(state->parse(cur->items[1], &val));
        auto def = new Ast::Definition(sym->text(), val);
        def->m_loc = sym->m_loc;
        state->addSym(sym->text(), def);
        seq->m_items.push_back(def);
    }
    Ast::Node* b;
    RETURN_IF_FAILED(state->parse(body, &b));
    seq->m_items.push_back(b);
    state->leaveScope();

    *out = seq;
    return Result::OK;
}

 Result Parse::If::_parse(State* state, Args& args, Ast::Node** out) const {
     RETURN_RES_IF(Result::ERR, args.size() != 3);
     Ast::Node* cond;
     RETURN_IF_FAILED(state->parse(args.cur(), &cond));
     args.advance();

     Ast::Node* iftrue;
     RETURN_IF_FAILED(state->parse(args.cur(), &iftrue));
     args.advance();

     Ast::Node* iffalse;
     RETURN_IF_FAILED(state->parse(args.cur(), &iffalse));
     args.advance();

     *out = new Ast::If(cond, iftrue, iffalse);
     return Result::OK;
 }

 Result Parse::Cond::_parse(State* state, Args& args, Ast::Node** out) const {
     *out = nullptr;
     RETURN_RES_IF(Result::ERR, args.size() < 1);
     std::vector< std::pair<Ast::Node*, Ast::Node*> > cases;
     for (auto arg : args) {
         auto pair = dynamic_cast<Lex::List*>(arg);
         RETURN_RES_IF(Result::ERR, pair == nullptr);
         RETURN_RES_IF(Result::ERR, pair->size() != 2);

         Ast::Node* cond;
         RETURN_IF_FAILED(state->parse(pair->items[0], &cond));
         Ast::Node* iftrue;
         RETURN_IF_FAILED(state->parse(pair->items[1], &iftrue));
         cases.emplace_back(cond, iftrue);
     }
     auto cond = new Ast::Cond;
     cond->m_cases.swap(cases);
     *out = cond;
     return Result::OK;
 }



 Result Parse::module( Lex::List* Lex, Ast::Module** out) {
    State state;
    state.addParser( "define", new Define() );
    state.addParser( "func", new Func() );
    state.addParser( "if", new If() );
    state.addParser( "cond", new Cond() );
    state.addParser( "let", new Let() );
    state.addSym( "int", &Ast::s_typeInt );
    state.addSym( "double", &Ast::s_typeDouble );
    state.addSym( "void", &Ast::s_typeVoid );
    state.addSym("eq_i?", new Ast::BinaryOperation("eq", new Ast::Argument("a", &Ast::s_typeInt), new Ast::Argument("b", &Ast::s_typeInt), &Ast::s_typeBool));
    state.addSym("lt_i?", new Ast::BinaryOperation("lt", new Ast::Argument("a", &Ast::s_typeInt), new Ast::Argument("b", &Ast::s_typeInt), &Ast::s_typeBool));
    state.addSym("add_i", new Ast::BinaryOperation("add", new Ast::Argument("a", &Ast::s_typeInt), new Ast::Argument("b", &Ast::s_typeInt), &Ast::s_typeInt));
    state.addSym("sub_i", new Ast::BinaryOperation("sub", new Ast::Argument("a", &Ast::s_typeInt), new Ast::Argument("b", &Ast::s_typeInt), &Ast::s_typeInt));
    state.addSym("true", new Ast::Argument("true", &Ast::s_typeBool));
    state.addSym("false", new Ast::Argument("false", &Ast::s_typeBool));

    auto module = new Ast::Module();
    for (auto c : Lex->items) {
        Ast::Node* n;
        RETURN_IF_FAILED(state.parse(c, &n));
        module->m_items.push_back(n);
    }
    *out = module;
    return Result::OK;
}

Result Parse::Parser::parse(State* state, Args& args, Ast::Node** out) const {
    return  _parse(state, args, out);
}
