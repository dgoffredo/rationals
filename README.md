![](logo.svg)

rationals
=========
This is a stripped down version of `boost::rational`, a class template that
implements rational arithmetic (fractions).

Why
---
A small, zero-dependencies `class rational<T>` might one day come in handy at
work.

What
----
See [rationals.h](rationals.h) and/or [the Boost documentation][1].

How
---
```c++
#include "rationals.h"
#include <cassert>
#include <cstdint>
#include <iostream>

int main() {
    using frac64_t = rationals::rational<std::int64_t>;

    const frac64_t not_pi(22, 7);

    std::cout << "pi is approximately " << not_pi << "\n";

    assert(not_pi > 3);
    assert(frac64_t(66, 21) == not_pi);
    assert(-frac64_t(-22, 7) == not_pi);
    assert(7 * not_pi == 22);
    assert(frac64_t(11, 17) / frac64_t(-1234, 5) == -frac64_t(55, 20978));
}
```

See the [unit test](rationals_test.C) for more.

[1]: http://www.boost.org/libs/rational 
