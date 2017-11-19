#pragma once
#include "Ast.h"

namespace Parse {

    struct State;
    typedef Iter<Lex::Atom*> Args;

    struct Parser {
        virtual ~Parser() {}
        virtual Ast::Node* parse(State* state, Args& args) const final;
    protected:
        virtual Ast::Node* _parse(State* state, Args& args) const = 0;
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

        Lex::Symbol* symbol(Lex::Atom* atom) {
            if (auto sym = dynamic_cast<Lex::Symbol*>(atom)) {
                return sym;
            }
            assert(0);
            return nullptr;
        }

        Ast::Type* _parseType(Lex::Atom* atom) {
            if( atom ) {
                auto p = parse(atom);
                assert(p);
                auto t = dynamic_cast<Ast::Type*>(p);
                assert(t);
                return t;
            }
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

    //protected:

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
        Ast::Node* _parse(State* state, Args& args) const override;
    };

    struct Func : public Parser {
        Ast::FunctionDecl* _parse(State* state, Args& args) const override;
    };

    struct Let : public Parser {
        Ast::Node* _parse(State* state, Args& args) const override;
    };
    
    Ast::Module* module(Lex::List* Lex);
}