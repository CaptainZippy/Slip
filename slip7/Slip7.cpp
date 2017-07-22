
// 1. lex + tree
// 2. tree shape check
// 3. Lex basics
// 4. Lex 

#include "Pch.h"
#include "SourceManager.h"
#include "Lex.h"
#include "Reflect.h"
#include "Ast.h"


namespace Sema {
    void collect_nodes(std::vector<Ast::Node*>& out, Reflect::Var var) {
        switch (var.type->kind) {
            case Reflect::Kind::Pointer: {
                if (var.type->sub->extends<Ast::Node>()) {
                    auto p = *static_cast<Ast::Node**>(var.addr);
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

    struct Lambda : public Parser {

        Ast::FunctionDecl* parse( State* state, Args& args ) const override {
            state->enterScope();
            auto ain = dynamic_cast<Lex::List*>(args.cur());
            args.advance();
            std::vector< Ast::Symbol* > arg_syms;
            for( auto i : ain->items ) {
                arg_syms.push_back( state->symbol( i ) );
            }

            Lex::Atom* rhs = args.cur();
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
        state.addParser( "lambda", new Lambda() );
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

int main( int argc, const char* argv[] ) {
    if( argc < 2 ) {
        Error.fmt( "Need a script to run" );
        return 1;
    }
    try {
        Lex::SourceManager smanager;
        Lex::List* Lex = Lex::parse_file( smanager, argv[1] );
        verify( Lex );
        Ast::Module* ast = Parse::module( Lex );
        verify(ast);
        Reflect::printVar(ast);
        Sema::type_check(ast);
        Reflect::printVar(ast);
    }
    catch( float ) {
        return 1;
    }
    return 0;
}
