// -*- Mode: C++; c-basic-offset: 2; indent-tabs-mode: nil -*-*/

// scratch: Fun things to do with constexpr

#include <iostream>

#include "peano.hh"

using namespace scratch::peano;

int main(void) {
  using _50 = product<_10, _5>;

  std::cout << to_ulong<_0>::value << std::endl;
  std::cout << to_ulong<_1>::value << std::endl;
  std::cout << to_ulong<_2>::value << std::endl;
  std::cout << to_ulong<_3>::value << std::endl;
  std::cout << to_ulong<_4>::value << std::endl;
  std::cout << to_ulong<_5>::value << std::endl;
  std::cout << to_ulong<_50>::value << std::endl;
}
