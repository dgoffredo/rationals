#include "rationals.h"

#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <limits>
#include <string>

// This file is not intended to be included in the build of any project that
// contains it. Instead, it's a separate, self-contained unit test driver. You
// can compile it into a binary executable, `rationals_test`, like this:
//
//     clang++-6.0 -o rationals_test -I. --std=c++11 rationals_test.C

// FUTURE: Adapt the original Boost unit test. That unit test is available (as
//         of this writing) at the following URL:
//
//     https://github.com/boostorg/rational/blob/
//         47c1b26964b06a99200769474d58a309267c50f1/test/rational_test.cpp

template <typename left_t, typename right_t>
void
assert_equal_impl(
    left_t&& left,
    const char* left_cpp,
    right_t&& right,
    const char* right_cpp,
    int line) {
    if (left == right) {
        return;
    }

    std::cerr << "Assertion failure on line " << line << ": " << left_cpp
              << " does not equal " << right_cpp << ", because " << left_cpp
              << " equals " << left << " while " << right_cpp << " equals "
              << right << "\n";
    std::abort();
}

#define assert_equal(left, right)                                              \
    assert_equal_impl(left, #left, right, #right, __LINE__)

void
multiplication() {
    using frac64_t = rationals::rational<std::int64_t>;

    assert_equal(frac64_t(1, 2) * frac64_t(1, 2), frac64_t(1, 4));
    assert_equal(frac64_t(1, 2) * frac64_t(1, 2), frac64_t(1, 4));

    const std::int64_t top = std::numeric_limits<std::int64_t>::max();
    assert_equal(frac64_t(top, 1) * frac64_t(1, top), 1);

    assert_equal(frac64_t(2, 3) * frac64_t(-3, 5), frac64_t(-2, 5));
}

void
addition() {
    using frac64_t = rationals::rational<std::int64_t>;

    assert_equal(frac64_t(1, 2) + frac64_t(1, 2), 1);
    assert_equal(frac64_t(1, 2) + frac64_t(1, 3), frac64_t(5, 6));
    assert_equal(frac64_t(1, 2) + frac64_t(-1, 2), 0);
    assert_equal(frac64_t(1, 10) + frac64_t(10, 1), frac64_t(101, 10));
}

void
normalization() {
    using frac64_t = rationals::rational<std::int64_t>;

    assert_equal(frac64_t(1337, 1337), 1);
    assert_equal(frac64_t(2, 4), frac64_t(1, 2));
    assert_equal(frac64_t(0, 5), frac64_t(0, 31415926));
}

void
breathing() {
    rationals::rational<int> pi_ish(22, 7);
    (void)pi_ish;
}

int
run_test(int which) {
    switch (which) {
    case 0:
        breathing();
        break;
    case 1:
        normalization();
        break;
    case 2:
        addition();
        break;
    case 3:
        multiplication();
        break;
    default:
        return -1;
    }

    return 0;
}

void
usage(std::ostream& out) {
    out << "usage: ./rationals_test [<index>]\n\n"
           "Run the unit test having the optionally specified index.\n"
           "If an index is not specified, then run all unit tests.\n"
           "Return status zero on success, or nonzero if a test fails.\n";
}

int
main(int argc, char* argv[]) {
    enum {
        TOO_MANY_ARGUMENTS = 1,
        INVALID_TEST_INDEX = 2
    };

    if (argc < 2) {
        for (int which = 0; run_test(which) == 0; ++which)
            ;
        std::cout << "all tests ran successfully\n";
        return 0;
    } else if (argc > 2) {
        usage(std::cerr);
        return TOO_MANY_ARGUMENTS;
    }

    std::string arg(argv[1]);
    if (arg == "-h" || arg == "--help") {
        usage(std::cout);
        return 0;
    } else if (run_test(std::stoi(arg)) == 0) {
        std::cout << "test " << arg << " ran successfully\n";
        return 0;
    }
    else {
        std::cerr << "invalid test index\n";
        return INVALID_TEST_INDEX;
    }
}
