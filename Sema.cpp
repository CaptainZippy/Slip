#include "pch/Pch.h"
#include "Ast.h"
#include "Sema.h"
#include "Io.h"

namespace Slip::Sema {

    struct TypeInfo;
    struct FuncInfo;
    struct ConstraintBuilder;

    struct FuncInfo {
        TypeInfo* ret{ nullptr };
        vector<TypeInfo*> args;
    };

    struct TypeInfo {
    private:
        friend struct ConstraintBuilder;
        Ast::Type* type{ nullptr };
        FuncInfo* func{ nullptr };//TODO: Union?
        int triggerCount{ 0 };
        std::vector<TypeInfo*> triggerNotify;
        std::function<void( TypeInfo* )> triggerAction;
    public:
        auto get_type() const {
            assert( type );
            return type;
        }
        auto get_func() const {
            assert( func );
            return func;
        }
    };

    struct VisitInfo {
        VisitInfo() = default;
        VisitInfo( Ast::Node* n, TypeInfo* t ) : node( n ), info( t ) {}
        Ast::Node* node{ nullptr };
        TypeInfo* info{ nullptr };
    };


    struct ConstraintBuilder {

        void operator()(Ast::Node* n, VisitInfo& vi) {
            assert(vi.info);
        }

        void operator()(Ast::Module* n, VisitInfo& vi ) {
            vi.info = _internKnownType(&Ast::s_typeVoid);//Todo: proper type
            for (auto i : n->m_items) {
                dispatch(i);
            }
        }

        void operator()(Ast::Number* n, VisitInfo& vi) {
            vi.info = _evalTypeExpr( n->m_declTypeExpr );
        }

        void operator()(Ast::String* n, VisitInfo& vi) {
            vi.info = _evalTypeExpr( n->m_declTypeExpr );
            //n->m_type = &Ast::s_typeString;
            //isa(n, &Ast::s_typeString);
            assert( false );
        }

        void operator()(Ast::FunctionCall* n, VisitInfo& vi) {
            auto fi = dispatch(n->m_func);
            std::vector<TypeInfo*> ai;
            for (auto&& a : n->m_args) {
                ai.emplace_back( dispatch(a) );
            }
            _isApplicable(fi, ai);
            vi.info = fi->get_func()->ret;
        }

        void operator()(Ast::Argument* n, VisitInfo& vi ) {
            vi.info = _evalTypeExpr( n->m_declTypeExpr );
        }

        void operator()(Ast::Sequence* n, VisitInfo& vi ) {
            if( n->m_items.empty() ) {
                vi.info = _internKnownType( &Ast::s_typeVoid );
            }
            else {
                for( auto&& i : n->m_items ) {
                    vi.info = dispatch( i );
                }
            }
        }

        void operator()(Ast::FunctionDecl* n, VisitInfo& vi) {
            if( n->m_type ) { // intrinsic?
                vi.info = _internKnownType( n->m_type );
            }
            else {
                vi.info = new TypeInfo{};
                auto ret = _evalTypeExpr( n->m_declReturnTypeExpr );
                std::vector<TypeInfo*> args;
                for( auto&& a : n->m_args ) {
                    args.emplace_back( dispatch( a ) );
                }
                _isFunction( vi.info, ret, std::move( args ) );
                if( auto bt = n->m_body ? dispatch( n->m_body ) : nullptr ) {
                    _isConvertible( ret, bt );
                }
            }
        }

        void operator()( Ast::VariableDecl* n, VisitInfo& vi ) {
            if( n->m_type ) { // known?
                vi.info = _internKnownType( n->m_type );
            }
            else if( n->m_initializer ) {
                auto ti = dispatch( n->m_initializer );
                vi.info = ti;
            }
            else {
                assert( false );
                vi.info = new TypeInfo{};
            }
        }

        void operator()(Ast::Reference* n, VisitInfo& vi ) {
            vi.info = dispatch(n->m_target);
        }

        void operator()(Ast::Scope* n, VisitInfo& vi ) {
            assert( false );
            //if (n->m_child) {
            //    //isa(n->m_child, n);
            //    dispatch(n->m_child);
            //}
            //else {
            //    isa(n, &Ast::s_typeVoid);
            //}
        }

        void operator()(Ast::Definition* n, VisitInfo& vi ) {
            assert( false );
            //isa(n, &Ast::s_typeVoid); //fixme
//            if (n->m_type) {
//                isa(n->m_value, n);
//            }
////            else {
//                isa(n, n->m_value);
//            }
            //dispatch(n->m_value);
        }

        void operator()(Ast::If* n, VisitInfo& vi ) {
            // condition is a boolean
            auto ci = dispatch( n->m_cond );
            _isConvertible( _internKnownType( &Ast::s_typeBool ), ci );
            // two legs have a common type
            auto ti = dispatch(n->m_true);
            auto fi = dispatch(n->m_false);
            vi.info = new TypeInfo{};
            _isConvertible( vi.info, ti );
            _isConvertible( vi.info, fi );
        }

        void operator()(Ast::Cond* n, VisitInfo& vi) {
            assert( !n->m_cases.empty() );
            vi.info = new TypeInfo{};
            for (auto&& c : n->m_cases) {
                // conditions are all booleans
                auto ft = dispatch(c.first);
                _isConvertible(_internKnownType(&Ast::s_typeBool), ft);
                // expressions convert to a common type
                auto st = dispatch(c.second);
                _isConvertible( vi.info, st );
            }
        }

    public:

        struct Convertible {
            Convertible(TypeInfo* b, TypeInfo* d) : base(b), derived(d) {}
            TypeInfo* base;
            TypeInfo* derived;
        };
        deque<VisitInfo> m_visited;
        unordered_map<Ast::Type*, TypeInfo*> m_knownTypes;
        std::vector< Ast::Type* > m_functionTypes;
        vector<Convertible> m_convertible;
        Ast::Module* m_module{ nullptr };

        Result build(Ast::Module* mod) {
            m_module = mod;
            return dispatch(mod) ? Result::OK : Result::ERR;
        }

        Result solve() {
            while( true ) {
                bool more = false; // backwards
                for( auto&& c : m_convertible ) {
                    if( !c.base->type && c.derived->type ) {
                        more = true;
                        _resolveType( c.base, c.derived->type );
                    }
                }
                if( !more ) {
                    more = false; // try forwards inference
                    for( auto&& c : m_convertible ) {
                        if( !c.derived->type && c.base->type ) {
                            more = true;
                            _resolveType( c.derived, c.base->type );
                        }
                    }
                    if( !more ) {
                        break;
                    }
                }
            }
            for( auto&& v : m_visited ) {
                assert( v.node );
                assert( v.info->type != nullptr );
                if( v.node->m_type ) {
                    assert( v.node->m_type == v.info->type );
                }
                else {
                    assert( v.node->m_type == nullptr );
                }

                v.node->m_type = v.info->type;
            }
            return Result::OK;
        }


        TypeInfo* dispatch(Ast::Node* top) {
            auto& i = top->m_userData;
            if( i >= m_visited.size() || m_visited[i].node != top ) {
                top->m_userData = m_visited.size();
                auto& vi = m_visited.emplace_back(top, (TypeInfo*)nullptr);
                Ast::dispatch<void>( top, *this, vi );
            }
            return m_visited[i].info;
        }

        ConstraintBuilder() {
        }

        ~ConstraintBuilder() {
        }

    protected:

        void _resolveType( TypeInfo* ti, Ast::Type* ty ) {
            ti->type = ty;
            for( auto&& a : ti->triggerNotify ) {
                assert( a->triggerCount > 0 );
                a->triggerCount -= 1;
                if( a->triggerCount == 0 ) {
                    (a->triggerAction)( a );
                }
            }
        }

        void _isConvertible( TypeInfo* base, TypeInfo* derived ) {
            m_convertible.emplace_back( base, derived );
        }

        TypeInfo* _internKnownType( Ast::Type* t ) {
            auto it = m_knownTypes.emplace( t, nullptr );
            if( it.second ) {
                auto ti = new TypeInfo;
                _resolveType( ti, t );
                it.first->second = ti;
                auto& call = t->m_callable;
                if( !call.empty() ) { // function type?
                    auto f = new FuncInfo;
                    ti->func = f;
                    f->ret = _internKnownType( call[0] );
                    for( auto&& a : array_view( call ).ltrim( 1 ) ) {
                        f->args.emplace_back( _internKnownType( a ) );
                    }
                }
            }
            return it.first->second;
        }

        TypeInfo* _evalTypeExpr( Ast::Node* te ) {
            if( !te ) {
                return new TypeInfo{};
            }
            if( auto t = dynamic_cast<Ast::Type*>( te ) ) {
                return _internKnownType( t );
            }
            else if( auto r = dynamic_cast<Ast::Reference*>( te ) ) {
                auto t = dynamic_cast<Ast::Type*>( r->m_target );
                assert( t );
                return _internKnownType( t );
            }
            assert( false );
            return nullptr;
        }

        void _buildFunctionType( TypeInfo* ti ) {
            assert( ti->type == nullptr );
            std::vector<Ast::Type*> args;
            FuncInfo* f = ti->func;
            args.emplace_back( f->ret->get_type() );
            for( auto&& a : f->args ) {
                args.emplace_back( a->get_type() );
            }
            for( auto&& ft : m_functionTypes ) {
                if( std::equal( ft->m_callable.begin() + 1, ft->m_callable.end() + 1, args.begin(), args.end() ) ) {
                    ti->type = ft;
                    return;
                }
            }
            // create new
            string name;
            name.append( "(" );
            const char* sep = "";
            for( auto a : f->args ) {
                name.append( sep ); sep = ", ";
                name.append( a->get_type()->m_name );
            }
            name.append( ") -> " );
            name.append( f->ret->get_type()->m_name );
            auto r = new Ast::Type( name );
            r->m_callable = std::move( args );
            m_functionTypes.emplace_back( r );
            _resolveType( ti, r );
        }

        void _addTriggerDep( TypeInfo* wall, TypeInfo* brick ) {
            if( brick->type == nullptr ) {
                wall->triggerCount += 1;
                brick->triggerNotify.push_back( wall );
            }
        }


        void _isFunction( TypeInfo* ti, TypeInfo* ret, vector<TypeInfo*>&& args ) {
            ti->func = new FuncInfo{ ret, args };
            _addTriggerDep( ti, ret );
            for( auto&& a : args ) {
                _addTriggerDep( ti, a );
            }
            if( ti->triggerCount == 0 ) {
                _buildFunctionType( ti );
            }
            else {
                ti->triggerAction = [this] ( TypeInfo* ti ) { _buildFunctionType( ti ); };
            }
        }

        void _isApplicable(TypeInfo* ti, array_view<TypeInfo*> args) {
            auto f = ti->get_func();
            assert( f );
            assert( f->args.size() == args.size() );
            for( unsigned i = 0; i < args.size(); ++i ) {
                _isConvertible( f->args[i], args[i] );
            }
        }
    };

    #if 0
    struct ConstraintSolver {
        Result dispatch(ConstraintBuilder& builder) {

            // apply explicit declarations in source
            for (auto&& target : builder.m_targets) {
                assert(target->type == nullptr);
                assert(target->node);
                if (auto t = target->node->m_type) {
                    target->resolve(t.get());
                }
                target = nullptr;
            }

            // 
            vector<TypeInfo*> todo = builder.m_targets;
            do {
                vector<TypeInfo*> again;
                for (auto&& target : todo) {
                    //HACK: if we didn't resolve and there is exactly one type constraint
                    // then lets take the constraint as the type.
                    bool resolved = false;
                    if (target->type == nullptr) {
                        if (target->isa.size()==1) {
                            if (auto&& t = target->isa[0]->type) {
                                target->resolve( t );
                                resolved = true;
                            }
                        }
                        else { //TODO: multiple intersecting constraints
                            set<Ast::Type*> types;
                            for_each(target->isa.begin(), target->isa.end(),
                                [&types](const TypeInfo* t) { types.insert(t->type); });
                            if (types.size() == 1) {
                                if (auto t = *types.begin()) {
                                    target->resolve(t);
                                    resolved = true;
                                }
                            }
                        }

                        if (resolved == false) {
                            again.push_back(target);
                        }
                    }
                }
                if (again.size() == todo.size()) {
                    Ast::print(builder.m_topNode);
                    for (auto&& a : again) {
                        a->print();
                    }
                    RETURN_RES_IF_REACHED(Result::ERR, "Failed to resolve");
                }
                again.swap(todo);
            } while (todo.size());
            print(builder);

            // type checks!
            for (auto&& target : builder.m_targets) {
                for (auto is : target->isa) {
                    assert(target->type == is->type);
                }
            }

            // Write results back to the ast
            for (auto&& target : builder.m_targets) {
                if (target->node->m_type.get() != target->type) {
                    target->node->m_type = target->type;
                }
            }

            return Result::OK;
        }
    protected:

        void print(ConstraintBuilder& builder) {
            Io::TextOutput io("deps.dgml");
            io.begin("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
            io.begin("<DirectedGraph xmlns=\"http://schemas.microsoft.com/vs/2009/dgml\">\n");
            io.begin("<Nodes>\n");
            for (auto&& src : builder.m_targets) {
                io.write(string_format("<Node Id='0x%p' Category='Node' Label='", src));
                io.write(src->node->dynamicType()->name);
                Ast::print(src->type, io);
                io.write("' />\n");
            }
            io.end("</Nodes>\n");
            io.begin("<Links>\n");
            for (auto&& src : builder.m_targets) {
                for (auto&& d : src->isa) {
                    io.write(string_format("<Link Source='0x%p' Target='0x%p' Category='Compatible'/>\n", src, d));
                }
            }
            /*for (auto syn : builder.m_synths) {
                io.write(string_format("<Link Source='0x%p' Target='0x%p' Category='SynthRet'/>\n", syn->target, syn->ret));
                for (auto d : syn->args) {
                    io.write(string_format("<Link Source='0x%p' Target='0x%p' Category='SynthArg'/>\n", syn->target, d));
                }
            }
            for (auto& syn : builder.m_checks) {
                io.write(string_format("<Link Source='0x%p' Target='0x%p' Category='CheckFunc'/>\n", syn->target, syn->func));
                for (auto a : syn->args) {
                    io.write(string_format("<Link Source='0x%p' Target='0x%p' Category='CheckArg'/>\n", syn->func, a));
                }
            }*/
            io.end("</Links>\n");
            io.begin("<Categories>\n");
            io.write("<Category Id=\"Compatible\"  StrokeDashArray=\"1\"/>\n");
            io.write("<Category Id=\"SynthArg\" StrokeThickness=\"3\" Stroke=\"#ff00ff00\" />\n");
            io.write("<Category Id=\"SynthRet\" StrokeThickness=\"3\" Stroke=\"#ffffff00\"/>\n");
            io.write("<Category Id=\"CheckArg\" StrokeThickness=\"1\" Stroke=\"#ff00ffff\"/>\n");
            io.end("</Categories>\n");
            io.end("</DirectedGraph>\n");
        }
    };
    #endif
}

void Slip::Sema::type_check(Slip::Ast::Module& mod) {
    ConstraintBuilder builder;
    //RETURN_IF_FAILED(
    builder.build(&mod);

    builder.solve();
    #if 0
    ConstraintSolver solver;
    ////RETURN_IF_FAILED
    solver.dispatch(builder);
    #endif
}

