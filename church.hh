// -*- Mode: C++; c-basic-offset: 2; indent-tabs-mode: nil -*-*/

// scratch: Fun things to do at compile time

#ifndef SCRATCH_CHURCH_HH
#define SCRATCH_CHURCH_HH

#include <typeinfo>

// Let's go to Church
// ==================

// Previously, we looked at Peano arithmetic, where we defined a bunch
// of arbitrary entities that got their meaning entirely through the
// relations we defined between them.

// Given that we've been writing in terms of function application with
// very little support from the type system, why not go the rest of
// the way and use nothing but function application? This will address
// one of the weaknesses of our previous effort. In peano.hh there was
// no good way to pass functions around. We could use template
// templates, but that still limits us. There are no template template
// templates, so we could never pass a function that itself takes a
// function to another function.

// Fortunately, there was once a man named Alonzo Church who, back in
// the 1920s, invented the Lambda Calculus. (It's only the Lambda
// calculus because his printer couldn't print underlined letters
// which was how he had been indicating the parameters to
// functions. Lambda was the second choice, and I'm grateful for this
// limited typography because The Underlined Calculus doesn't sound
// nearly as good.) He believed that it, like the more famous Turing
// machine, was a model capable of performing all computation that
// could be performed. Turing agreed, which is why you hear of the
// Church-Turing thesis. (The Lambda Calculus predates the Turing Machine,
// though they were discovered independently.)

// In the Lambda calculus, everything is a function. Numbers, pairs,
// lists, everything. Thus, passing functions around, rather than the
// exception, will be the rule with no exceptions at all.
// Therefore, let's use a convention for how we will build our
// functions in the template system.

// A function will be an untemplated type (a struct, in particular)
// containing a templated type called 'apply'. Yes! Member types can
// be templated even when the containing type isn't. Isn't that cool?
//
// So, to apply a function, F, to a single argument, X, we simply use:
//
//    F::apply<X>
//
// The instantiated type of F::apply is the result of the function.

// So, let's make some numbers.


// Church Numerals
// ---------------
//
// Church numerals are functions of two arguments. They apply their
// first argument to their second argument some number of times. Which
// number? Exactly the number that they are.

// As usual, we start with zero, but zero is less conceptually
// important for Church numerals than for declarative Peano
// numerals. For declarative Peano numbers, five's fiveness was
// entirely derived from being the successor to the successor to the
// successor to the successor to the successor of zero and that's
// it. In Church numerals, any function of two arguments that applies
// its first argument to its second argument five times is five. Zero
// is merely the zeroth among equals, not the keystone of the arch of
// numerical reality.

// Zero, as you can see, applies its first value to its second value
// zero times.
struct _0 {
  template<typename F, typename X>
  using apply = X;
};

// This is the successor operation. We don't have to define everything
// in terms of successors, but it's an easy and natural way to do so,
// so we're going to.
struct S {
  template<typename N>
  struct impl {
    template<typename F, typename X>
    // The syntax here is horrible, by the way. Apply is a dependent
    // type, so you have to include the typename keyword, and you're
    // using it as a template so you have to include the template
    // keyword. I wonder if I could use some macros to fix this.
    using apply = typename F::template apply<typename N::template apply<F, X>>;
  };
  // This is necessary to get around a limitation in ISO C++ about one
  // type containing a type with the same name as itself.
  template<typename N>
  using apply = impl<N>;
};

// Here we build addition. This is very neat, nice, and natural. Well,
// the concepts are neat, nice, and natural. The syntax is still
// atrocious.
//
// As you can see, we take advantage of the fact that all of our
// numbers are counted iterating functions. To add one number.
//
// n + m = Sⁿ(m)
struct sum {
  template<typename Augend, typename Addend>
  using apply = typename Addend::template apply<S, Augend>;
};

// Multiplication is our most complicated function by far! That's
// because we have to curry a function. Well, we have to partially
// apply it. Currying is /most properly/ the operation of turning a
// function with several arguments into a function that takes one
// argument and returns another function that takes one argument and
// so on until you get to the result. Calling partial application
// currying is an abuse of terminology I picked up from the Scheme
// community. I will have to see if there's a way to curry template
// arguments. I have a vague idea how to go about it with parameter
// packs and some support machinery, but...
//
// Conceptually, of course, it's still quite simple. Our curried_sum
// function is just the two-argument sum function from above with one
// of the arguments filled in with the multiplicand. After that it's
// just like sum and successor: we apply the multiplier to it and zero
// to build up the answer.
struct product {
  template<typename Multiplicand, typename Multiplier>
  struct impl {
    struct curried_sum {
      template<typename N>
      using apply = typename sum::apply<Multiplicand, N>;
    };
    using exec = typename Multiplier::template apply<curried_sum, _0>;
  };
  template<typename Multiplicand, typename Multiplier>
  using apply = typename impl<Multiplicand, Multiplier>::exec;
};

// Exponentiation is identical to multiplication except that we swap
// product out for sum and use one as the base case instead of
// zero. Oh. I forgot to define one.
using _1 = S::apply<_0>;

// There we are.
struct power {
  template<typename Base, typename Exponent>
  struct impl {
    struct curried_product {
      template<typename N>
      using apply = typename product::apply<Base, N>;
    };
    using exec = typename Exponent::template apply<curried_product, _1>;
  };
  template<typename Base, typename Exponent>
  using apply = typename impl<Base, Exponent>::exec;
};

// Now we can make LOTS of numbers
using _2 = S::apply<_1>;
using _3 = S::apply<_2>;
using _4 = S::apply<_3>;
using _5 = S::apply<_4>;
using _6 = S::apply<_5>;
using _7 = S::apply<_6>;
using _8 = S::apply<_7>;
using _9 = S::apply<_8>;

// But that's kind of boring so let's not, at least for now.

// You might be wondering why we don't have subtraction. Subtraction
// is hard. Even Alonzo Church couldn't figure out how to subtract
// Church numerals. Stephen Kleene (he of the famed Kleene star) had
// to figure it out. Specifically, he made the Predecessor function.

// In the Peano numbers, I cheated and stuck the Predecessor in every
// number, so implementing things like subtraction was quite
// easy. This worked because, in that system, there was one unique
// type that was five. In fact, that's why I would have been able to
// implement a predecessor function even if I hadn't cheated; I could
// always count up from zero, saving the previous step, and when I got
// to a type equal to the one supplied, return the one before that.

// That won't work in Church numerals since there can be several
// unrelated types that all represent five, and there is no way,
// generally, to check function equality. In fact, I can't implement
// equality at all without the predecessor function or something like it.

// Obviously, we have work to do....

// But people go to Church to learn about Truth, right?

// Truth and Falsehood
// -------------------
//
// Church booleans are— surprise, surprise— also functions. If you
// have ever programmed in Smalltalk they will look familiar.

// True is a function of two arguments that returns the first.
struct True {
  template<typename Consequent, typename Alternative>
  using apply = Consequent;
};

// False is a function of two arguments that returns the second.
struct False {
  template<typename Consequent, typename Alternative>
  using apply = Alternative;
};

// And that's it. We need no if statement, just the booleans
// themselves. …but some Boolean operators might be nice.

// Boolean Operators
// -----------------

// Sorry, and is a reserved word.
struct conj {
  template<typename A, typename B>
  using apply = typename A::template apply<B, False>;
};

// And so is or
struct disj {
  template<typename A, typename B>
  using apply = typename A::template apply<True, B>;
};

// And so is not. For the love of Pete.
struct neg {
  template<typename A>
  using apply = typename A::template apply<False, True>;
};

// Pairs
// -----
//
// A pair (which we will later see is also a cons cell) is a function
// of one argument. It takes a function of two arguments and applies
// it to its contents.

struct Pair {
  template<typename First, typename Second>
  struct impl {
    // This should be fairly straight forward, but! We make impl a
    // template, so that everything inside is also dependent on the
    // template parameters. In the semantics of our language, this
    // makes the function we return close over (or capture) the types
    // it depends on.
    template<typename F>
    using apply = typename F::template apply<First, Second>;
  };
  template<typename First, typename Second>
  using apply = impl<First, Second>;
};

// We can extract things from pairs with True and False like so:
//
//   Pair::apply<First, Second><True> =
//   True::apply<First, Second> = First
//
// and
//
//   Pair::apply<First, Second>::apply<False> =
//   False::apply<First, Second> = Second

// Predecessor
// -----------
//
// We are now ready to tread the path closer to zero that Kleene
// blazed.

// If we have the number N, N::apply<S, _0> = N. Kleene's realization
// was that we just need to call the successor function /one less
// time/. We just have to wrap N and the successor function up in a
// bit of machinery to make that happen.
//
// This means that in canonical Church numerals P::apply(_0) = _0 which
// removes the worry of crashing through the floor that we had in our
// Peano implementation.
struct P {
  template<typename N>
  struct impl {
    // Here we wrap our data. The first element tells us whether we've
    // been called yet.
    using init = Pair::apply<False, _0>;

    // This function just takes our wrapped tuple and returns it with
    // the first value set to true.
    struct skip {
      template<typename A>
      using apply = Pair::apply<True, typename A::template apply<False>>;
    };
    // This function performs the actual increment.
    struct actual {
      template<typename A>
      using apply = Pair::apply<True,
                                S::apply<typename A::template apply<False>>>;
    };
    // And this function combines the two to make our function that
    // increments our wrapped value except for the first time it's
    // called.
    struct inc {
      template<typename A>
      using apply = typename A::template apply<True>
        ::template apply<actual, skip>::template apply<A>;
      // The rules C++ has about marking things out as dependent types
      // and templates if they aren't defined are really obnoxious.
    };

    // And now we put it all together.
    using exec = typename N::template apply<inc, init>;

    // Don't forget, inc returns a pair, so we need to apply it to
    // False to get the second element which is the actual number.
    using unwrap = typename exec::template apply<False>;
  };
  template<typename N>
  using apply = typename impl<N>::unwrap;
};

// Subtraction
// -----------
//
// We've done a lot of work to get to this point, but now that we have
// the Predecessor function, subtraction is as easy as division.

// Subtraction isn't commutative and we need to track our left and
// right arguments, which is why I've been using these archaic names
// for the operands. Oh, you thought I was doing it because I just
// like the way they sound? People can use archaic language for more
// than one reason!

// Due to the definition of Predecessor, if Subtrahend is larger than
// Minuend, the result will be zero, rather than ⊥.
struct difference {
  template<typename Minuend, typename Subtrahend>
  using apply = typename Subtrahend::template apply<P, Minuend>;
};


// Testing
// -------
//
// Can we test things without bringing them into the normal world of
// C++ values? Sure! We just need to make failure...fail. An easy way
// to do that is to evaluate an expression that can't be evaluated.

struct assert {
  template<typename B>
  struct impl {
    // An identity function...of one argument.
    struct one_arg {
      template<typename O>
      using apply = O;
    };

    // An identity function...of two arguments.
    struct two_args {
      template<typename O, typename Q>
      using apply = O;
    };
    using pick = typename B::template apply<one_arg, two_args>;
    using exec = typename pick::template apply<_0>;
  };
  template<typename B>
  using apply = typename impl<B>::exec;
};

// Predicates
// ----------
//
// Now that we have truth, falsehood, predecessor, and assert, it
// might be good to write some predicates so we can start testing our
// code before we go any further.

// Since _0 is the only number that returns its second argument
// unmodified, we simply need to pass True in as the second argument
// and a function that always returns False as the first. Don't
// forget, we actually do need to define a test for zero explicitly
// since any number of functions can be zero.
struct zero {
  struct constantly_false {
    template<typename>
    using apply = False;
  };

  template<typename N>
  using apply = typename N::template apply<constantly_false, True>;
};

// Given the above definition of subtraction, we get a definition of
// less-than-or-equal essentially for free.
struct leq {
  template<typename M, typename N>
  using apply = zero::apply<difference::apply<M, N>>;
};

// And since, as we all know:
//
// x ≤ y ∧ y ≤ x ↔ x = y
//
struct equal {
  template<typename M, typename N>
  using apply = conj::apply<leq::apply<M, N>, leq::apply<N, M>>;
};

// And a simple less than in terms of both.
struct less {
  template<typename M, typename N>
  using apply = conj::apply<leq::apply<M, N>,
                            neg::apply<equal::apply<N, M>>>;
};

namespace tests {
using zerotest = assert::apply<zero::apply<_0>>;
using predtest = assert::apply<zero::apply<P::apply<_1>>>;
using leqtest = assert::apply<leq::apply<_2, _5>>;
using multtest = assert::apply<
  equal::apply<product::apply<_2, _5>, S::apply<_9>>>;
using powertest = assert::apply<equal::apply<power::apply<_3, _2>, _9>>;
};

#endif // SCRATCH_CHURCH_HH
