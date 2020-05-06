#include "pch/Pch.h"

#include "Ast.h"
#include "Lex.h"
#include "Parse.h"
#include "Slip.h"

using namespace Slip;

namespace {
    using namespace std;
    using namespace Slip;
    struct Evaluator {

        struct Value {

        };
        Result dispatch( Ast::Node* n, Ast::Environment* env, Ast::Node** out ) {
            Value v;
            return Ast::dispatch<Result>( n, *this, env, v );
        }

        Result operator()( Ast::Node* n, Ast::Environment* env, Value& v ) {
            assert( false );
            return Result::OK;
        }

        Result operator()( Ast::Number* n, Ast::Environment* env, Value& v ) { return Result::OK; }

        Result operator()( Ast::Reference* n, Ast::Environment* env, Value& v ) {
            return dispatch( n->m_target, env, nullptr );
            return Result::OK;
        }

        Result operator()( Ast::FunctionDecl* n, Ast::Environment* env, Value& v ) {
            return Result::OK;
        }

        Result operator()( Ast::FunctionCall* n, Ast::Environment* env, Value& v ) {
            Ast::Node* func;
            RETURN_IF_FAILED( dispatch( n->m_func, env, &func ) );
            vector<Ast::Node*> args;
            for( auto a : n->m_args ) {
                Ast::Node* n;
                RETURN_IF_FAILED( dispatch( a, env, &n ) );
                args.push_back( n );
            }
            return Result::OK;
        }
    };
}  // namespace

Result Eval::evaluate( Ast::Environment* env, Ast::Node* node, Ast::Node** out ) {
    *out = nullptr;
    Sema::type_check( node );

    return Evaluator{}.dispatch( node, env, out );
}
