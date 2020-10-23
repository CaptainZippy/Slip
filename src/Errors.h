#pragma once

namespace Slip::Error {
    enum {
        Continued = -1, // special case - additional information to the previous error
        NoError = 0,
#define ERROR_CASE( A ) A,
#include "Errors.inc"
#undef ERROR_CASE
    };
}  // namespace Error
