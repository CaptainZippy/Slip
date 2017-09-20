#pragma once
#include "Lex.h"

namespace Parse {

    struct State;
    typedef Iter<Lex::Atom*> Args;

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

        Ast::Symbol* symbol(Lex::Atom* atom) {
            if (auto sym = dynamic_cast<Lex::Symbol*>(atom)) {
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

        Ast::Node* parse(Lex::Atom* atom) {
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
            else if (auto num = dynamic_cast<Lex::Number*>(atom)) {
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

    struct Func : public Parser {

        Ast::FunctionDecl* parse( State* state, Args& args ) const override {
            auto func = new Ast::FunctionDecl();
            func->m_name = state->symbol(args.cur());
            args.advance();
            state->addSym(func->m_name->text(), func);
            state->enterScope();
            auto ain = dynamic_cast<Lex::List*>(args.cur());
            args.advance();
            for( auto i : ain->items ) {
                func->m_arg_syms.push_back( state->symbol( i ) );
            }

            Lex::Atom* rhs = args.cur();
            args.advance();
            verify( args.used() );

            for( auto a : func->m_arg_syms ) {
                state->addSym( a->m_name, new Ast::Argument(a) );
            }
            func->m_body = state->parse( rhs );
            state->leaveScope();

            return func;
        }
    };

    struct Let : public Parser {
        Ast::Scope* parse( State* state, Args& args ) const override {
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
    
    Ast::Module* module( Lex::List* Lex ) {
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
}