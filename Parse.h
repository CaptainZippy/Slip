#pragma once
#include "Ast.h"
#include "Lex.h"

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

        void addParser(string_view sym, Parser* value) {
            auto s = istring::make(sym);
            auto p = syms.back().insert_or_assign(s, Pair{ value, nullptr } );
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
            assert(0);
            return nullptr;
        }

        Result _parseType(Lex::Atom* atom, Ast::Type** out) {
            if (atom == nullptr) { *out = nullptr; return Result::OK; }
            auto sym = dynamic_cast<Lex::Symbol*>(atom);
            RETURN_RES_IF(Result::ERR, sym == nullptr);
            const Pair* p;
            RETURN_IF_FAILED(lookup(sym->text(), &p));
            RETURN_RES_IF(Result::ERR, p->first != nullptr);
            auto t = dynamic_cast<Ast::Type*>(p->second);
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
        std::list< std::map<istring, Pair> > syms;
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
