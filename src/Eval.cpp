#include "pch/Pch.h"

#include "Ast.h"
#include "Slip.h"

using namespace Slip;

namespace {
    using namespace std;
    using namespace Slip;
    struct Evaluator {
        enum class OpCode { Add };

        struct Stack {
            enum class Kind { Invalid = 0, Code, Int };
            struct Value {
                Value( Kind k, intptr_t v ) : kind_( k ), val_( v ) {}
                Kind kind_;
                intptr_t val_;
            };
            typedef std::vector<OpCode> ByteCode;

            size_t size() const { return vals_.size(); }

            void pushInt( intptr_t l ) { vals_.emplace_back( Kind::Int, l ); }
            void pushCode( const ByteCode* l ) { vals_.emplace_back( Kind::Code, (intptr_t)l ); }

            // const ByteCode* toByteCode( int idx ) { vals_.emplace_back( Kind::Code, (intptr_t)l ); }
            // intptr_t toInt( int idx ) { vals_.emplace_back( Kind::Code, (intptr_t)l ); }

            Value& at( int i ) { return i < 0 ? vals_[vals_.size() + i] : vals_[i]; }

            Result call( int n ) {
                assert( at( -n - 1 ).kind_ == Kind::Code );
                auto ops = (const std::vector<OpCode>*)at( -n - 1 ).val_;
                for( auto op : *ops ) {
                    switch( op ) {
                        case OpCode::Add: {
                            assert( at( -2 ).kind_ == Kind::Int );
                            assert( at( -1 ).kind_ == Kind::Int );
                            at( -3 ) = {Kind::Int, at( -2 ).val_ + at( -1 ).val_};
                            vals_.pop_back();
                            vals_.pop_back();
                            break;
                        }
                        default: { assert( false ); }
                    }
                }
                return Result::OK;
            }

            std::vector<Value> vals_;
        };
        Stack stack_;

        std::map<Ast::FunctionDecl*, std::vector<OpCode> > funcs_;

        Result dispatch( Ast::Node* n, Ast::Environment* env ) { return Ast::dispatch<Result>( n, *this, env ); }

        Result operator()( Ast::Node* n, Ast::Environment* env ) {
            assert( false );
            return Result::OK;
        }

        Result operator()( Ast::Number* n, Ast::Environment* env ) {
            assert( n->m_type == &Ast::s_typeInt );
            auto s = n->m_num.data();
            auto e = s + n->m_num.size();
            auto i = strtoll( s, &e, 10 );  // todo error
            stack_.pushInt( i );
            return Result::OK;
        }

        Result operator()( Ast::Reference* ref, Ast::Environment* env ) {
            RETURN_IF_FAILED( dispatch( ref->m_target, env ) );
            return Result::OK;
        }

        Result operator()( Ast::FunctionDecl* n, Ast::Environment* env ) {
            auto it = funcs_.find( n );
            if( it == funcs_.end() ) {
                std::vector<OpCode> code;
                code.emplace_back( OpCode::Add );
                auto p = funcs_.emplace( n, code );
                assert( p.second );
                it = p.first;
            }
            stack_.pushCode( &it->second );
            return Result::OK;
        }

        // Result callFunc( const Value& vfunc, const vector<Value>& args ) {
        //    assert( vfunc.kind_ == ValueKind::FunctionDecl );
        //    auto func = vfunc.func_;
        //    assert( func->m_args.size() == args.size() );

        //    return Result::OK;
        //}

        Result operator()( Ast::FunctionCall* n, Ast::Environment* env ) {
            RETURN_IF_FAILED( dispatch( n->m_func, env ) );
            for( auto a : n->m_args ) {
                RETURN_IF_FAILED( dispatch( a, env ) );
            }
            return stack_.call( (int)n->m_args.size() );
        }

        Result operator()( Ast::NamedFunctionCall* n, Ast::Environment* env ) {
            RETURN_ERROR_IF( n->m_resolved == nullptr, Error::UnresolvedCall, n->m_loc );
            RETURN_IF_FAILED( dispatch( n->m_resolved, env ) );
            return Result::OK;
        }
    };
}  // namespace

#define WITH( ... ) [&]( auto& _ ) { __VA_ARGS__; }

Result Eval::evaluate( Ast::Environment* env, Ast::Node* node, Ast::Node** out ) {
    *out = nullptr;
    Sema::type_check( node );

    Evaluator ev;
    // Evaluator::Value val;
    ev.dispatch( node, env );
    assert( ev.stack_.size() == 1 );
    switch( ev.stack_.at( 0 ).kind_ ) {
        case Evaluator::Stack::Kind::Int: {
            static char foo[111];
            int n = snprintf( foo, sizeof( foo ), "%zu", ev.stack_.at( 0 ).val_ );
            auto v = istring::make( foo, n );
            *out = new Ast::Number( v, WITH( _.m_type = &Ast::s_typeInt ) );
            return Result::OK;
            break;
        }
        default:
            assert( false );
            break;
    }
    return Result::ERR;
}
