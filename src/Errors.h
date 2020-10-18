#pragma once

namespace Slip::Error {
    enum {
        NoError = 0,
#define ERROR_CASE( A ) A,
#include "Errors.inc"
#undef ERROR_CASE
    };
}  // namespace Error
