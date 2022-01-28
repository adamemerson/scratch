// -*- Mode: C++; c-basic-offset: 2; indent-tabs-mode: nil -*-*/

// scratch: Fun things to do with constexpr

#include "tree.hh"

constexpr int plus(int x, int y) {
  return x + y;
}

int main() {
  constexpr auto one = scratch::make_leaf(1);
  constexpr auto two = scratch::make_leaf(2);
  constexpr auto three = scratch::make_leaf(3);
  constexpr auto four = scratch::make_leaf(4);
  constexpr auto five = scratch::make_leaf(5);
  constexpr auto six = scratch::make_leaf(6);

  constexpr auto t = (one ^ two) ^ ((three ^ four) ^ (five ^ six));

  constexpr auto l = scratch::reduce_leaves(t, plus);
  constexpr auto c = scratch::count_leaves<decltype(t), int>::value;

  static_assert(l == 21, "Reduce failure.");
  static_assert(c == 6, "Count failure.");

  return 0;
}
