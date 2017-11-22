#include "Pch.h"
#include "Parse.h"

Ast::Node* Parse::State::parse(Lex::Atom* atom) {
    if (auto sym = dynamic_cast<Lex::Symbol*>(atom)) {
        auto p = lookup(sym->text());
        verify(p.first == nullptr);
        return reference(p.second);
    }
    else if (auto list = dynamic_cast<Lex::List*>(atom)) {
        assert(list);
        assert(list->items.size());
        auto& items = list->items;
        auto sym = symbol(items[0]);
        Args args{ items }; args.advance();
        auto p = lookup(sym->text());
        if (p.first) { // a builtin
            return p.first->parse(this, args);
        }
        else { 
            std::vector<Ast::Node*> fa;
            for (auto a : args) {
                fa.push_back(parse(a));
            }
            return new Ast::FunctionCall(reference(p.second), std::move(fa));
        }
    }
    else if (auto num = dynamic_cast<Lex::Number*>(atom)) {
        return new Ast::Number(num);
    }
    verify(0);
    return nullptr;
}

Ast::Node* Parse::Define::_parse( State* state, Args& args ) const  {
    auto sym = state->symbol( args.cur() );
    args.advance();
    auto val = state->parse( args.cur() );
    args.advance();
    verify( args.used() );
    auto ret = new Ast::Definition( sym, val );
    state->addSym( sym->text(), ret );
    return ret;
}

Ast::FunctionDecl* Parse::Func::_parse( State* state, Args& args ) const {
    auto func = new Ast::FunctionDecl();
    func->m_name = state->symbol(args.cur());
    args.advance();
    state->addSym(func->m_name->text(), func);
    state->enterScope();
    for( auto item : dynamic_cast<Lex::List*>(args.cur())->items ) {
        auto sym = state->symbol(item);
        verify(sym);
        auto arg = new Ast::Argument(sym);
        arg->m_type = state->_parseType(item->m_decltype);
        func->m_args.push_back( arg );
    }
    args.advance();

    Lex::Atom* rhs = args.cur();
    args.advance();
    verify( args.used() );

    for( auto a : func->m_args ) {
        state->addSym( a->m_sym->text(), a );
    }
    func->m_body = state->parse( rhs );
    state->leaveScope();

    return func;
}

Ast::Node* Parse::Let::_parse( State* state, Args& args ) const {
    auto lets = dynamic_cast<Lex::List*>( args.cur() );
    args.advance();
    auto body = args.cur();
    args.advance();
    verify(args.used());

    state->enterScope();
    auto seq = new Ast::Sequence();
    for( auto pair : lets->items) {
        auto cur = dynamic_cast<Lex::List*>( pair );
        verify(cur->size() == 2);
        auto sym = dynamic_cast<Lex::Symbol*>(cur->items[0]);
        verify(sym);
        auto val = state->parse(cur->items[1]);
        auto def = new Ast::Definition(sym, val);
        state->addSym(sym->text(), def);
        seq->m_items.push_back(def);
    }
    seq->m_items.push_back(state->parse(body));
    state->leaveScope();

    return seq;
}
    
Ast::Module* Parse::module( Lex::List* Lex ) {
    State state;
    state.addParser( "define", new Define() );
    state.addParser( "func", new Func() );
    state.addParser( "let", new Let() );
    state.addSym( "int", &Ast::s_typeInt );
    state.addSym( "double", &Ast::s_typeDouble );
    state.addSym( "void", &Ast::s_typeVoid );
    auto module = new Ast::Module();
    for (auto c : Lex->items) {
        Ast::Node* n = state.parse(c);
        module->m_items.push_back(n);
    }
    return module;
}

Ast::Node* Parse::Parser::parse(State* state, Args& args) const {
    return  _parse(state, args);
}
