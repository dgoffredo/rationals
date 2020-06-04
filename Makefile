rationals_test: rationals_test.C rationals.h
	clang++-6.0 -o $@ --std=c++11 -Wall -Wextra -Wpedantic -Werror $<

.PHONY: clean
clean:
	rm -f rationals_test
