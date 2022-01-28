// -*- Mode: C++; c-basic-offset: 2; indent-tabs-mode: nil -*-*/

// scratch: Fun things to do at compile time

#ifndef SCRATCH_CONDITIONAL_HH
#define SCRATCH_CONDITIONAL_HH

namespace scratch {
  // When last we met, we were implementing the operations of basic
  // arithmetic over the natural numbers. All seemed well, until we
  // tried to implement division and discovered that, due to having no
  // notion of ordering and no way to implement a conditional
  // expression, we couldn't divide safely.

  // Sure, we could use a base case of zero and repeatedly subtract,
  // but if the dividend wasn't an exact multiple of the divisor, we'd
  // crash through the bottom of the natural numbers.

  // Surely there must be some hope for us.

  // Surely there must be a....
  namespace conditional {
    // I was going to name it 'namespace bool' but then I realized
    // that was a reserved word.
    namespace detail {

      // TRUTH!
      struct True {};

      // FALSEHOOD!
      struct False {};

      // They're both just empty types. But they're /disjoint/ from
      // each other and from everything else. So long as our
      // predicates and our conditionals agree on which things are
      // true and which things are false, we'll be fine.

      // And here is our conditional. It's quite simple. First, we
      // have our generic template declaration. We don't strictly need
      // it. We could specify a generic one and set type to Consequent
      // in it and then specialize on False so that everything
      // not-False is true. Or the other way around.

      // But since this stuff is loosey-goosey enough I'd much rather
      // have an If with something that isn't a truth value just fail.
      template<typename Predicate, typename Consequent, typename Alternate>
      struct _If { };

      // And here is the specialization for True. We set our type to
      // the Consequent. The 'then' part of an if statement.
      template<typename Consequent, typename Alternate>
      struct _If<True, Consequent, Alternate> {
        using type = Consequent;
      };

      // And this is our equivalent to 'else'.
      template<typename Consequent, typename Alternate>
      struct _If<False, Consequent, Alternate> {
        using type = Alternate;
      };

      // Templates aren't properly first-class types. I can't pass
      // 'If' into something directly. (I can make it a member of some
      // OTHER struct and pass that in.) So template metaprogramming
      // doesn't /exactly/ have first-class functions. If it did, we
      // wouldn't even need an 'if' statement. We could have our true
      // and false values take two arguments and set their result type
      // to the appropriate one.

      // This is actually what smalltalk does. Boolean values take one
      // or two 'block' arguments (basically thunks). True evaluates
      // the first and false evaluates the second.
      template<typename Predicate, typename Consequent, typename Alternate>
      using If = typename _If<Predicate, Consequent, Alternate>::type;

      // And an Or are most naturally defined as variadic templates.

      template<typename... Bs>
      struct _And { };

      // If the first argument is true, try the rest.
      template<typename... Bs>
      struct _And<True, Bs...> {
        using type = typename _And<Bs...>::type;
      };

      // If the first argument is false, then we're false.
      template<typename... Bs>
      struct _And<False, Bs...> {
        using type = False;
      };

      // In the case of /no/ arguments, we return true. True is the
      // identity for the And operation.
      template<>
      struct _And<> {
        using type = True;
      };

      template<typename... Bs>
      using And = typename _And<Bs...>::type;

      template<typename... Bs>
      struct _Or { };

      // If the first argument is true, then we're true
      template<typename... Bs>
      struct _Or<True, Bs...> {
        using type = True;
      };

      // If the first argument is false, then try the rest
      template<typename... Bs>
      struct _Or<False, Bs...> {
        using type = typename _Or<Bs...>::type;
      };

      // In the case of /no/ arguments, we return false. False is the
      // identity for the Or operation.
      template<>
      struct _Or<> {
        using type = False;
      };

      template<typename... Bs>
      using Or = typename _Or<Bs...>::type;

      // Again an empty general case for Not.
      template<typename T>
      struct _Not {};

      template<>
      struct _Not<True> {
        using type = False;
      };

      template<>
      struct _Not<False> {
        using type = True;
      };

      template<typename T>
      using Not = typename _Not<T>::type;

      template<typename... Bs>
      using Nand = Not<And<Bs...> >;

      template<typename... Bs>
      using Nor = Not<Or<Bs...> >;

      template<typename P, typename Q>
      struct _Xor { };

      template<>
      struct _Xor<False, False> {
        using type = False;
      };

      template<>
      struct _Xor<False, True> {
        using type = True;
      };

      template<>
      struct _Xor<True, False> {
        using type = True;
      };

      template<>
      struct _Xor<True, True> {
        using type = False;
      };

      template<typename P, typename Q>
      using Xor = typename _Xor<P, Q>::type;

      // I don't expect anyone to actually use this, but why not?
      template<typename P, typename Q>
      using Implies = Or<Not<P>, Q>;

      // Or this.
      template<typename P, typename Q>
      using Iff = Not<Xor<P, Q> >;

      // And definitely not this
      template<typename P, typename Q>
      using ConverseImplies = Or<P, Not<Q> >;

      // Equality belongs here with the truth and falsehood and the
      // logical operators. Why? Because to see if two types are
      // equal, we just see if the two types are equal, taking
      // advantage of template substitution ordering.

      // In our general case, just return false.
      template<typename A, typename B>
      struct _Eq {
        using type = False;
      };

      // Since this template is more specific, if the compiler can
      // possibly satisfy it, it will.
      template<typename A>
      struct _Eq<A, A> {
        using type = True;
      };

      template<typename A, typename B>
      using Eq = typename _Eq<A, B>::type;

      // Again, not necessary for actually evaluating things, but it's
      // nice if we ever want to print something.
      template<typename T>
      struct to_bool {};

      template<>
      struct to_bool<True> {
        static constexpr bool value = true;
      };

      template<>
      struct to_bool<False> {
        static constexpr bool value = false;
      };

      // Since we have to_bool we can just test everything with static_assert.
      struct test {
        static_assert(to_bool<True>::value, "");
        static_assert(to_bool<Not<False> >::value, "");

        static_assert(to_bool<If<True, True, False> >::value, "");
        static_assert(to_bool<Not<If<False, True, False> > >::value, "");

        static_assert(to_bool<And<> >::value, "");
        static_assert(to_bool<And<True> >::value, "");
        static_assert(to_bool<Not<And<False> > >::value, "");

        static_assert(to_bool<Not<And<False, True> > >::value, "");

        static_assert(to_bool<And<True, True> >::value, "");

        static_assert(to_bool<Not<Or<> > >::value, "");
        static_assert(to_bool<Or<True> >::value, "");
        static_assert(to_bool<Not<Or<False> > >::value, "");

        static_assert(to_bool<Or<False, True> >::value, "");

        static_assert(to_bool<Or<True, True> >::value, "");

        static_assert(to_bool<Eq<int, int> >::value, "");
        static_assert(to_bool<Not<Eq<int, True> > >::value, "");
      };
    }

    using detail::True;
    using detail::False;
    using detail::If;
    using detail::And;
    using detail::Or;
    using detail::Not;
    using detail::Nand;
    using detail::Nor;
    using detail::Xor;
    using detail::Implies;
    using detail::Iff;
    using detail::ConverseImplies;
    using detail::Eq;
    using detail::to_bool;
  }
}

#endif // SCRATCH_CONDITIONAL_HH
