// -*- Mode: C++; c-basic-offset: 2; indent-tabs-mode: nil -*-*/

// scratch: Fun things to do with constexpr

#include <iostream>

#include "list.hh"

constexpr int square(int x) {
  return x * x;
}

// std::plus isn't constexpr‽ This makes me unhappy

constexpr int plus(int x, int y) {
  return x + y;
}

constexpr bool even(int x) {
  return !(x & 1);
}

int main(void) {
  constexpr auto list = scratch::make_list(1, 2, 3, 4, 5);
  constexpr auto squares = scratch::map(list, &square);
  static_assert(squares == scratch::make_list(1, 4, 9, 16, 25),
                "Map failure.");
  static_assert((scratch::foldr(list, 0, &plus) == 15) &&
                (scratch::foldl(list, 0, &plus) == 15),
                "Fold failure.");
  static_assert(scratch::reverse(squares) ==
                scratch::make_list(25, 16, 9, 4, 1),
                "Reverse failure.");
  constexpr auto listu64 = scratch::make_list(1ULL, 2ULL, 3ULL, 4ULL, 5ULL);
  static_assert(list == listu64, "Equality failure.");
  static_assert(listu64 == list, "Equality failure.");
  static_assert(list != scratch::make_list(1, 2, 3, 4),
                "Equality failure.");
  static_assert(scratch::count(list, &even) == 2,
                "Count failure.");
  static_assert(scratch::append(list, squares) ==
                scratch::make_list(1, 2, 3, 4, 5, 1, 4, 9, 16, 25),
                "Count failure.");

  constexpr auto z1 = scratch::zip(
    list, scratch::make_list(true, false, true, false, true));

  static_assert(
    z1 == scratch::make_list(std::make_tuple(1, true),
                             std::make_tuple(2, false),
                             std::make_tuple(3, true),
                             std::make_tuple(4, false),
                             std::make_tuple(5, true)),
    "Zip failure.");

  return 0;
}
