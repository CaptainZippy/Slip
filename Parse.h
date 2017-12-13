#pragma once
#include "Ast.h"

namespace Parse {

    struct State;
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

        void addParser(const std::string& sym, Parser* value) {
            auto p = syms.back().insert_or_assign(sym, Pair{ value, nullptr } );
            assert(p.second);
        }

        void addSym(const std::string& sym, Ast::Named* node) {
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

        Result _parseType(Lex::Atom* atom, Ast::Type** out) {
            auto sym = dynamic_cast<Lex::Symbol*>(atom);
            RETURN_RES_IF(Result::ERR, sym == nullptr);
            auto p = lookup(sym->text());
            RETURN_RES_IF(Result::ERR, p.first != nullptr);
            auto t = dynamic_cast<Ast::Type*>(p.second);
            RETURN_RES_IF(Result::ERR, t == nullptr);
            *out = t;
            return Result::OK;
        }

        Result parse(Lex::Atom* atom, Ast::Node** out);

    //protected:

        typedef std::pair<Parser*, Ast::Named*> Pair;
        Ast::Reference* reference(Ast::Named* n) {
            return new Ast::Reference(n);
        }

        Pair lookup(const std::string& sym) const {
            for (auto&& cur : reversed(syms)) {
                auto x = cur.find(sym);
                if (x != cur.end()) {
                    return x->second;
                }
            }
            assert(0);
            return Pair{};
        }
        std::list< std::map<std::string, Pair> > syms;
    };

    struct Define : public Parser {
        Result _parse(State* state, Args& args, Ast::Node** out) const override;
    };

    struct Func : public Parser {
        Result _parse(State* state, Args& args, Ast::Node** out) const override;
    };

    struct Let : public Parser {
        Result _parse(State* state, Args& args, Ast::Node** out) const override;
    };

    struct If : public Parser {
        Result _parse(State* state, Args& args, Ast::Node** out) const override;
    };
    
    Result module(Lex::List* Lex, Ast::Module** out);
}
