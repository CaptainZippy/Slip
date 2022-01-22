
// Register multiple 'main' functions so that we amortize
// the slow part of building (link time)
struct MainReg {
    typedef int ( *mainfunc )( int argc, const char** argv );
    MainReg( const char* n, mainfunc m );
    static const MainReg* head; // head of null terminated list
    const char* name; // name - often the basename of the file containing
    mainfunc main; // The function to call
    const MainReg* next;
};

const MainReg* MainReg::head{};
MainReg::MainReg( const char* n, MainReg::mainfunc m ) : name( n ), main(m), next( head ) { head = this; }

#include <cstdio>
#include <cstring>

int main(int argc, const char** argv) {
    const char* testname{nullptr};
    // Find the registered main function and pass on remaining arguments to it.
    if( argc > 1 ) {
        testname = argv[1];
        for( auto p = MainReg::head; p; p = p->next ) {
            if( strcmp( p->name, testname ) == 0 ) {
                return (*p->main)( argc - 1, argv + 1 );
            }
        }
        std::printf( "Test '%s' not found\n\n", testname );
    }
    std::printf( "These tests are registered:\n" );
    for( auto p = MainReg::head; p; p = p->next ) {
        std::printf( "\t%s\n", p->name );
    }
    return 1;
}
