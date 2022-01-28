// -*- Mode: C++; c-basic-offset: 2; indent-tabs-mode: nil -*-*/

// scratch: Fun things to do at compile time

#ifndef SCRATCH_PEANO_HH
#define SCRATCH_PEANO_HH

#include "conditional.hh"

namespace scratch {
  // Come on, let's have the courage of our convictions!

  // So, the type system knows about integers. You can do add and
  // subtract and multiply abd divide in template parameters and the
  // like. So it's perfectly fair to use regular old integers in a
  // type based system. And I could ue std::integral_constant as a
  // nice, neat way of giving every number its own type. I could even
  // implement my own analogue with slightly friendlier syntax. But I
  // figured, just for fun, why not implement arithmetic from scratch?

  // No negative numbers. We can implement the classic construction of
  // the integers later if we want.
  namespace peano {
    namespace detail {
      // This is zero. There's nothing special about it other than
      // that it's zero. All the other natural numbers are defined in
      // terms of it.
      struct _0 { };

      // This is the successor operation. It's the fundamental
      // relation upon which arithmetic is built.
      template<typename N>
      struct S {
        using P = N;
      };

      using _1 = S<_0>;
      using _2 = S<_1>;
      using _3 = S<_2>;
      using _4 = S<_3>;
      using _5 = S<_4>;
      using _6 = S<_5>;
      using _7 = S<_6>;
      using _8 = S<_7>;
      using _9 = S<_8>;
      using _10 = S<_9>;

      // You'll notice I really like using declarations to get more
      // friendly names for things. Template metaprogramming is
      // verbose as heck and I do what I can to cut it down.

      // This is just an alias. Since every number contains all
      // numbers before it, we can just reach in and grab the
      // predecessor. I like having the more uniform prefix notation.
      template<typename N>
      using P = typename N::P;

      // The sum!
      //
      // Formally:
      // a + 0 = a
      // a + S(b) = S(a) + b
      template<typename Augend, typename Addend>
      struct _sum {
        // Incidentally, if you type that's a member of some other
        // type, you have to prefix it with typename. The compiler
        // will complain if you don't, and it's one of the few
        // template related error messages that's easy to understand.
        using type = typename _sum<S<Augend>, P<Addend> >::type;
      };

      // As you should know by now, especially if you looked through
      // list.hh, more specific templates win out over less specific
      // templates. So we specify our base case thus.
      template<typename Augend>
      struct _sum<Augend, _0> {
        using type = Augend;
      };

      // Because I don't like to type typename sum<X, Y>::type
      // repeatedly.
      template<typename Augend, typename Addend>
      using sum = typename _sum<Augend, Addend>::type;

      // Subtraction! These are natural numbers, don't forget. If you
      // try to subtract something non-zero from zero, you'll get a
      // type error due to zero being the successor of no natural number.

      // Formally:
      // a - 0 = a
      // S(a) - S(b) = a - b
      template<typename Minuend, typename Subtrahend>
      struct _difference {
        using type = typename _difference<P<Minuend>, P<Subtrahend> >::type;
      };

      template<typename Minuend>
      struct _difference<Minuend, _0> {
        using type = Minuend;
      };

      template<typename Minuend, typename Subtrahend>
      using difference = typename _difference<Minuend, Subtrahend>::type;

      // Multiplication! This is just repeated addition. Or, more
      // formally:
      //
      // a * 0 = 0
      // a * S(b) = a + (a * b)
      template<typename Multiplicand, typename Multiplier>
      struct _product {
        using type = sum<Multiplicand,
                         typename _product<Multiplicand,
                                           P<Multiplier> >::type >;
      };

      template<typename Multiplicand>
      struct _product<Multiplicand, _0> {
        using type = _0;
      };

      template<typename Multiplicand, typename Multiplier>
      using product = typename _product<Multiplicand, Multiplier>::type;

      // No division! Not yet. For natural number division we need
      // comparison and conditionals. So let's import some conditionals.

      using conditional::True;
      using conditional::False;
      using conditional::If;
      using conditional::Eq;
      using conditional::Not;

      // Well, okay, that was easy. Now that we know what true and
      // fals are, let's define comparison.

      // We can determine whether A is less than B recursively because
      //
      // S(a) < S(b) ⇔ a < b
      template<typename A, typename B>
      struct _less {
        using type = typename _less<P<A>, P<B> >::type;
      };

      // And

      // a ≠ 0 ⇔ 0 < a
      template<typename B>
      struct _less<_0, B> {
        using type = True;
      };

      template<typename A>
      struct _less<A, _0> {
        using type = False;
      };

      template<>
      struct _less<_0, _0> {
        using type = False;
      };

      template<typename A, typename B>
      using less = typename _less<A, B>::type;

      // And now that we have comparison, we can define division,
      // recursively. Bear in mind that all of our numbers are
      // unary. We can't use things like Long Division that rely on
      // our numbers already having been conveniently broken down into
      // sums of products of ten or a hundred or a thousand. Normal
      // computer systems use a variant of long division based on the
      // numbers beng broken up into sums of powers of two, just by
      // virtue of being stored in a digital memory. So we work by
      // repeated subtraction.

      // This is a pair type. It doesn't actually belong in this file,
      // but we use it here and it seems a bit precious to define a
      // file /just/ for the pair type. At least until we use it in
      // more than one place.
      template<typename First, typename Second>
      struct pair {
        using first = First;
        using second = Second;
      };

      // We need that pair type because computing the quotient gives
      // us the remainder, automatically, and vice versa.


      // The defining relations that govern our division are:
      //
      // a = bq + r
      // 0 ≤ r < b

      // We can easily derive our recursive formula, like so:

      // a < b ⇔ a = r ∧ q = 0
      // a = b S(q) + r ⇔ a - b = bq + r

      // Or, in other words, keep subtracting b from a, and add a one
      // to your eventual quotient each time until a is less than
      // b. Then bottom out and return our quotient, and whatever is
      // left of a.
      template<typename Dividend, typename Divisor, typename Quotient = _0>
      struct _euclidean_division {
        // We actually don't need If here, we could just pattern match
        // explicitly on true or false. SOmetimes this syntax is
        // nicer.
        using type = If<
          less<Dividend, Divisor>,
          pair<Quotient, Dividend>,
          typename _euclidean_division<difference<Dividend, Divisor>,
                                       Divisor,
                                       S<Quotient> >::type>;
        using quotient = typename type::first;
        using remainder = typename type::second;
      };

      // One of the downsides of conditionals evaluating both forks is
      // that we have to put in explicit base cases like this.
      template<typename Divisor, typename Quotient>
      struct _euclidean_division<_0, Divisor, Quotient> {
        using type = pair<Quotient, _0>;
        using quotient = typename type::first;
        using remainder = typename type::second;
      };

      template<typename Dividend, typename Divisor>
      using euclidean_division = typename _euclidean_division<
        Dividend, Divisor>::type;

      template<typename Dividend, typename Divisor>
      using quotient = typename _euclidean_division<
        Dividend, Divisor>::quotient;

      template<typename Dividend, typename Divisor>
      using remainder = typename _euclidean_division<
        Dividend, Divisor>::remainder;

      // Of course we could also just add a 'value' element to zero
      // and our successor operation, but I wanted to make the point
      // that we're building numbers out of nothing, and sticking
      // unsigned long integers in there would somewhat spoil the effect.

      // The template metalanguage is not type safe. You could apply
      // S<to_ulong<_5> > and you would get back a type with a P
      // element in it. But it would effectively be its own '1' with
      // to_ulong<5> as a new '0'. Except none of the other template
      // functions would work on it since they all have the unique
      // type _0 as their base case. You'd just end up with the type
      // system failing during compilation.
      template<typename N>
      struct to_ulong {
        static constexpr unsigned long value = 1 + to_ulong<P<N> >::value;
      };

      template<>
      struct to_ulong<_0> {
        static constexpr unsigned long value = 0;
      };

      using conditional::to_bool;
      struct test {
        static_assert(to_bool<Not<less<_10, _3> > >::value, "");
        static_assert(to_bool<less<_3, _10> >::value, "");
        static_assert(to_bool<Not<less<_3, _3> > >::value, "");
        static_assert(to_bool<Not<less<_0, _0> > >::value, "");
        static_assert(to_bool<Eq<quotient<_6, _3>, _2> >::value, "");
        static_assert(to_bool<Eq<remainder<_6, _3>, _0> >::value, "");
      };
    }

    using detail::_0;
    using detail::S;
    using detail::P;
    using detail::sum;
    using detail::difference;
    using detail::product;
    using detail::to_ulong;
    using detail::less;
    using detail::pair;
    using detail::euclidean_division;
    using detail::quotient;
    using detail::remainder;

    using detail::_1;
    using detail::_2;
    using detail::_3;
    using detail::_4;
    using detail::_5;
    using detail::_6;
    using detail::_7;
    using detail::_8;
    using detail::_9;
    using detail::_10;
  }
}

#endif // SCRATCH_PEANO_HH
