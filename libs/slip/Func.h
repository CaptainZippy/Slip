#include <memory>

namespace Slip {
    template <class Sig>
    class Func;

    namespace details {

        template <class Ret, class... Args>
        struct func_pimpl {
            virtual Ret invoke( Args&&... args ) const = 0;
            virtual ~func_pimpl() {}
        };

        // store an F.  invoke(Args&&...) calls the f
        template <class Fun, class Ret, class... Args>
        struct func_pimpl_impl : func_pimpl<Ret, Args...> {
            Fun fun_;

            template <class Fin>
            func_pimpl_impl( Fin&& fin ) : fun_( std::forward<Fin>( fin ) ) {}

            virtual Ret invoke( Args&&... args ) const final override { return fun_( std::forward<Args>( args )... ); }
        };

        // the void version discards the return value of f:
        template <class Fun, class... Args>
        struct func_pimpl_impl<Fun, void, Args...> : func_pimpl<void, Args...> {
            Fun fun_;

            template <class Fin>
            func_pimpl_impl( Fin&& fin ) : fun_( std::forward<Fin>( fin ) ) {}

            virtual void invoke( Args&&... args ) const final override { fun_( std::forward<Args>( args )... ); }
        };
    };  // namespace details

    // https://riptutorial.com/cplusplus/example/18042/a-move-only--std--function-

    template <class Ret, class... Args>
    class Func<Ret( Args... )> {
       private:
        // aliases to make some SFINAE code below less ugly:
        template <class F>
        using call_r = std::invoke_result_t<F, Args...>;
        template <class F>
        using is_func = std::is_same<std::decay_t<F>, Func>;

       public:
        Func() = default;
        Func( Func&& ) = default;

        Func( const Func& ) = delete;
        void operator=( const Func& ) = delete;
        Func& operator=( Func&& ) = default;

        template <class Fun,                                                 // can be constructed from a callable F
                  class = decltype( (Ret)( std::declval<call_r<Fun>>() ) ),  // that can be invoked with Args... and converted-to-R:
                  std::enable_if_t<!is_func<Fun>{}, int>* = nullptr>         // and is not this same type:
        Func( Fun&& fun ) : m_pImpl( make_pimpl( std::forward<Fun>( fun ) ) ) {}

        // the meat: the call operator
        Ret operator()( Args... args ) const { return m_pImpl->invoke( std::forward<Args>( args )... ); }

        explicit operator bool() const { return (bool)m_pImpl; }

        void swap( Func& o ) { std::swap( m_pImpl, o.m_pImpl ); }

        template <class Fun>
        void assign( Fun&& fun ) {
            m_pImpl = make_pimpl( std::forward<Fun>( fun ) );
        }

        // compare with nullptr    :
        friend bool operator==( std::nullptr_t, Func const& self ) { return !self; }
        friend bool operator==( Func const& self, std::nullptr_t ) { return !self; }
        friend bool operator!=( std::nullptr_t, Func const& self ) { return !!self; }
        friend bool operator!=( Func const& self, std::nullptr_t ) { return !!self; }

       private:
        using base_pimpl_t = details::func_pimpl<Ret, Args...>;
        template <class T>
        using exact_pimpl_t = details::func_pimpl_impl<T, Ret, Args...>;

        template <class F>
        static auto make_pimpl( F&& f ) {
            using dF = std::decay_t<F>;
            using impl_t = exact_pimpl_t<dF>;
            return std::make_unique<impl_t>( std::forward<F>( f ) );
        }
        std::unique_ptr<base_pimpl_t> m_pImpl;
    };
}  // namespace Slip
