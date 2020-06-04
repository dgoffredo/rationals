#include "rationals.h"

#include <cstdlib>
#include <iostream>
#include <string>

void
breathing() {
    rationals::rational<int> pi_ish(22, 7);
    std::cout << "pi is approximately " << pi_ish << "\n";
}

int
run_test(int which) {
    switch (which) {
    case 0:
        breathing();
        break;
    default:
        return -1;
    }

    return 0;
}

void
usage(std::ostream& out) {
    out << "usage: ./test [<index>]\n\n"
           "Run the unit test having the optionally specified index.\n"
           "If an index is not specified, then run all unit tests.\n"
           "Return status zero on success, or nonzero if a test fails.\n";
}

int
main(int argc, char* argv[]) {
    if (argc < 2) {
        for (int which = 0; run_test(which) == 0; ++which)
            ;
        return 0;
    } else if (argc > 2) {
        usage(std::cerr);
        return 1;
    }

    std::string arg(argv[1]);
    if (arg == "-h" || arg == "--help") {
        usage(std::cout);
        return 0;
    } else {
        run_test(std::stoi(arg));
        return 0;
    }
}
