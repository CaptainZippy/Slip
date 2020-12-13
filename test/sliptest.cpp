#include "pch.h"

#include "CppUnitTest.h"
#include "slip/Slip.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

namespace sliptest {
    static Slip::Result compile( const char* fname ) {
        auto path = std::string{"../../../test/"} + fname + ".slip";
        const char* argv[2] = {"slip", path.c_str()};
        return Slip::Main::main( 2, argv );
    }

    TEST_CLASS( feature ) {
       public:
#define TEST(X) TEST_METHOD( X ) { Assert::IsTrue( compile( "feature/" #X ).isOk() ); }
#include "../test/feature/tests.inc"
#undef TEST
    };

    TEST_CLASS( errors ) {
       public:
#define TEST( X ) \
    TEST_METHOD( X ) { Assert::IsFalse( compile( "errors/" #X ).isOk() ); }
#include "../test/errors/tests.inc"
#undef TEST
    };
}  // namespace sliptest
