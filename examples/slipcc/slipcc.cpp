#include "../../src/Slip.h"

int main( int argc, const char* argv[] ) {
    if( Slip::Main::main( argc, argv ).isOk() ) {
        return 0;
    }
    return 1;
}
