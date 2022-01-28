// -*- Mode: C++; c-basic-offset: 2; indent-tabs-mode: nil -*-*/

// scratch: Fun things to do with constexpr

#ifndef SCRATCH_LIST_HH
#define SCRATCH_LIST_HH

#include <cstdint>
#include <iostream>
#include <tuple>
#include <type_traits>

// This is something I put together because I wanted to have a
// semi-realistic example of something one could do with constexpr
// under C++11 that gives a good demonstration of its capabilities and
// limitations.

namespace scratch {
  namespace detail {
    // This is a list. Not because it's objects pointing to eachother
    // one after the other, but because it has a head (the thing in a
    // given element of the list) and a tail (the rest of the list)
    // and terminates with a nil at the end.

    // To have a variable-sized, first-class LiteralType object,
    // objects of different lengths must have different types. One
    // could, potentially, make a LiteralType linked list by
    // allocating each link on its own in static storage or on the
    // stack (in C++14) in constexpr functions, but this is /not/
    // first class; you can't have functions that take a list and
    // return another list. I had wondered if one could, in C++14, use
    // a fixed-size array as an arena and erase the type difference
    // for variable length objects that way, but the restriction on
    // modifying objects outside of the expression in which their
    // lifetime began rules that out.

    // This approach is similar to (but not exactly the same as) how
    // you make tuples.

    // If you don't know anything about templates, this file is either
    // an excellent introduction or a terrible introduction. You can
    // supply an actual LiteralType as a template argument (such as
    // size_t here.) in which case users of the template supply actual
    // values (or they're inferred.) If the type of the argument is
    // 'typename' (or 'class' but I don't care for that usage), then
    // an actual type is supplied or inferred. So, in this case, we
    // have a list of type T (where T can be an integer or float or
    // struct or whatever you like) and length N. The type system
    // doesn't know N is the length, it just knows that our type is
    // list<N, T>.

    // We define our list recursively. A list of length N is just a
    // list of length N - 1 with one more element, and that's
    // basically what inheriting from list<N - 1, T> does. We use
    // private inheritance since we don't want things lower down in
    // the hierarchy to show up.
    template<std::size_t N, typename T>
    class list : private list<N - 1, T> {
    public:
      // is_nil and length aren't really ever going to be used, since
      // the recursive template matching always gets there first.
      using is_nil = std::false_type;
      using value_type = T;
      using length = std::integral_constant<std::size_t, N>;

      // Recursively constructs a list, element by
      // element. Technically I /could/ stick an enable_if on here and
      // check that all Ts in the pack are compatible types, but if I
      // do that I just end up getting a long, complicated template
      // error. And if the user messes up, I get a long, complicated
      // template error. So there's no reason to bother. Besides, use
      // make_list, it's safer.

      // This is a variadic template, by the way. The 'typename...'
      // introduces what is called a parameter pack. Since it's
      // 'typename' the template accepts an unbounded number of
      // different types which are then inserted into the argument
      // list. The '(rest...)' is the parameter pack expansion. That
      // means that we call our parent constructor with all the
      // arguments we receive except the first one.
      template<typename... Ts>
      constexpr list(const T& datum, const Ts&... rest)
        : list<N - 1, T>(rest...), datum(datum) {}

      // This is the actual machinery of cons. We accept a list one
      // element shorter than the one we're constructing and just copy
      // it (with the implicit copy constructor) and set our head to
      // the datum provided. We define an operator further on down as
      // syntactic sugar.
      constexpr list(const T& datum, const list<N - 1, T>& l)
        : list<N - 1, T>(l), datum(datum) {}

      // Return a reference to what we're storing.
      constexpr const T& head() const {
        return datum;
      }

      // This is how we get to the rest of the list. Since each
      // iteration of the object is just stacked piece by piece, if we
      // statically cast to our direct ancestor we get its head and
      // its tail.

      // I was tempted to name these 'car' and 'cdr', but we don't
      // have any equivalent to a dotted pair. And the whole thing is
      // immutable anyway so I went with Haskell flavored notation.

      // It feels right, too, since the recursive template matching
      // feels a lot like pattern matching.
      constexpr list<N - 1, T> tail() const {
        return static_cast<list<N - 1, T>>(*this);
      }

      // This is the equality operator. It's templated and accepts a
      // list of length N (that is, just as long as the object on the
      // left hand side) and any type. (As you might have guessed, the
      // template parameters N and U have scope extending over the
      // entire class. So the 'N' in the argument below is the same as
      // the 'N' defining things class.)
      //
      // There's no need to restrict the function, all we care about
      // is that there exists an equality operator between type T and
      // type U (or something they can be implicitly coerced into). If
      // there isn't the compiler will fail with a type error, which
      // is what's supposed to happen when you try to compare two
      // things that don't have an equality relation defined.
      //
      // It is also recursive, like the constructor. Two lists are
      // equal if their heads are equal and their tails are
      // equal. Funny that...shouldn't there be a terminating
      // condition or something?
      template<typename U>
      constexpr bool operator ==(const list<N, U>& rhs) const {
        return (head() == rhs.head()) && (tail() == rhs.tail());
      }

      // Because having to type !(a == b) all the time is a pain. We
      // just define this to be the opposite of equality.
      template<typename U>
      constexpr bool operator !=(const list<N, U>& rhs) const {
        return !(*this == rhs);
      }

      // As I said before, lists of different lengths are different
      // types and the compiler treats them as such. If we didn't
      // define this, trying to compare a list of five integers to a
      // list of four integers would give a type error. This just says
      // that any list with a different length than ours (which to the
      // type checker just means any value of type list<M, T> where
      // M ≠ N, is not equal to this one.

      // You might be asking me, "Crazy template programmer, where
      // does it say that M ≠ N? You don't have any constraints on
      // this thing." I don't have to have an explicit constraint
      // because C++ has a specific ordering for template
      // substitution. It goes for the most specific templates first
      // and then moves to more general ones. The equality operator
      // above has only one free variable, U, the length of the list
      // on the right hand side is the same as that on the left. So
      // the compiler will prefer that substitution before it tries
      // this one that has two free variables.
      template<std::size_t M, typename U>
      constexpr bool operator ==(const list<M, U>&) const {
        return false;
      }

      // Same as above, except the opposite, since this is inequality.
      template<std::size_t M, typename U>
      constexpr bool operator !=(const list<M, U>&) const {
        return true;
      }

    private:
      value_type datum;
    };

    // Here's a specialization of the above. Note that specialization
    // is not inheritance. The contents of the class can be whatever I
    // want. As you can see, the head and tail methods are missing
    // completely, because nil has neither one.
    template<typename T> class list<0, T> {
    public:
      using is_nil = std::true_type;
      using length = std::integral_constant<std::size_t, 0>;
      using value_type = T;

      constexpr list(){};

      // Remember that terminating condition that wasn't there? It's
      // here. Eventually, every list traces its ancestry up to a
      // list<0, T>. When list<1, T>::tail returns a list<0, T> we get
      // to this function which always returns true.
      template<typename U>
      constexpr bool operator ==(const list<0, U>&) const {
        return true;
      }

      // Similarly, something that isn't nil can't be equal to nil.
      template<std::size_t M, typename U>
      constexpr bool operator ==(const list<M, U>&) const {
        return false;
      }

      // Not a base case, but good for consistency.
      template<typename U>
      constexpr bool operator !=(const list<0, U>&) const {
        return false;
      }

      // And again, something not nil should be unequal to nil.
      template<std::size_t M, typename U>
      constexpr bool operator !=(const list<M, U>&) const {
        return true;
      }
    };

    // This is just convenient. I like it because 'nil' is more
    // visually distinct than list<0,T>.
    template<typename T>
    using nil = list<0, T>;

    // I originally called this 'cons' but since I went with head and
    // tail over car and cdr, I figured I may as well pick an infix |
    // over a prefix cons as the closest thing to a colon I can get in
    // C++.
    template<std::size_t N, typename T>
    constexpr list<N + 1, T> operator |(const T& d, const list<N, T>& l) {
      // Since the compiler knows we're returning a list<N + 1, T>
      // this brace notation tells it to find a constructor that can
      // be called with the stuff inside them and call it.
      return {d, l};
    }

    // As mentioned above, make_list is safer. Why? Because it
    // determines the length of the list from the number of arguments
    // you pass it. Then you can just use auto to have the compiler
    // determine the type of the variable.
    template<typename T, typename... Ts>
    constexpr auto make_list(T el, Ts... els)
    // The 'sizeof...' here is not the same thing as the sizeof()
    // operator. With a 'parameter pack' (that's what they call a
    // variable number of types that hasn't been expanded) it gives
    // the number of parameters in the pack. In this case we take T
    // and a parameter pack. So our list should be 1 + the number of
    // parameters in the pack.
      -> list<1 + sizeof...(Ts), T> {
      // And here we expand the pack. We pass the first argument we
      // were supplied followed by all the rest to the
      // constructor. Which is coincidentally symmetric to how the
      // constructor works.
      // The only reason we separated them like that was so we could
      // use the type of the first argument as the type of the
      // list. This does have the downside that if I supply, say, '5'
      // as my first argument, and ~1ULL as my second argument, I'll
      // have a problem because my list will have been inferred to be
      // a list of ints.
      return {el, els...};
    }

    // If we are given no arguments, make the empty list. I don't know
    // why you would do this, since you'd have to specify the type
    // explicitly and just calling nil<T>() directly would be easier.
    template<typename T>
    constexpr nil<T> make_list() {
      return nil<T>();
    }

    // Occasionally I/O is a nice thing to do. This is a helper
    // function that prints a list, in the general case. Generally,
    // to print a list, print the head, a comma, a space, and
    // the rest of the list.
    template<std::size_t N, typename T>
    void list_print_helper(std::ostream& m, const list<N, T>& l) {
      m << l.head() << ", ";
      list_print_helper(m, l.tail());
    }

    // And here we have a template specialization. Since this is more
    // specific than the one above, it will be preferred whenever it
    // can be applied. You see what I mean? This whole thing feels
    // like really verbose, ugly pattern matching.

    // A list of one element should be printed by just printing the
    // element. (No need to recurse to the base case.)
    template<typename T>
    void list_print_helper(std::ostream& m, const list<1, T>& l) {
      m << l.head();
    }

    // A list of no elements doesn't need to print anything.
    template<typename T>
    void list_print_helper(std::ostream&, const nil<T>&) {
      return;
    }

    // And here's the wrapper. Since I wanted to bracket lists I
    // separated this out from the 'helper'. We just print a starting
    // bracket, call the helper to print the list proper, and then
    // print a closing bracket.
    template<std::size_t N, typename T>
    std::ostream& operator<<(std::ostream& m, const list<N, T>& l) {
      m << "[";
      list_print_helper(m, l);
      return m << "]";
    }

    // This is a textbook fold. Since this is my library, I can call
    // it /fold/. Not /accumulate/. Do you know how annoying it was
    // every time I'd try to find 'fold' in the standard library and
    // it wasn't there?

    // This is implemented fairly intuitively, we just recursively
    // apply the function with the head as the first argument and the
    // fold of the tail of the list as the second, on the right.

    // Right folds will exhaust your compiler's constexpr recursion
    // limit if you make them too deep. 512 by default on G++ and
    // Clang++, but you can expand them. If you expand it too much on
    // G++ and then recurse over it you crash the compiler.
    template<typename T, std::size_t N, typename S, typename Fun>
    constexpr S foldr(const list<N, T>& l, S seed, Fun&& fun) {
      return fun(l.head(), foldr(l.tail(), seed, fun));
    }

    // Until we hit the base case.
    template<typename T, typename S, typename Fun>
    constexpr S foldr(const list<0, T>&, S seed, Fun&&) {
      return seed;
    }

    // This is a left fold. In principle, because the recursive call
    // is in tail position, the compiler should be able to iterate
    // through it as often as it wants. Unfortunately no C++ compiler
    // on the market implements tail-call optimization in constexpr.

    // This is similar to the right fold, except the recursive call to
    // fold is on the left. Funny, that.
    template<typename T, std::size_t N, typename S, typename Fun>
    constexpr S foldl(const list<N, T>& l, S seed, Fun&& fun) {
      return foldl(l.tail(), fun(seed, l.head()), fun);
    }

    // And it has the same base case. Notice above that foldl is 'top
    // down' where 'foldr' is bottom up. The topmost call to foldl
    // uses the seed in its application of f and supplies the result
    // as the seed to the next iteration. Right fold, on the other
    // hand, waits for the entire thing to bottom out and return the
    // seed before its first application of the function can return.
    template<typename T, typename S, typename Fun>
    constexpr S foldl(const list<0, T>&, S seed, Fun&&) {
      return seed;
    }

    // I think /map/ is a much better name than /transform/
    // too. Seriously. This takes an existing list and creates a new
    // list by applying a function to each element.

    template<typename T, std::size_t N, typename Fun>
    constexpr auto map(const list<N, T>& l, Fun&& fun)
      -> list<N, typename std::result_of<Fun(T)>::type> {
      // Now it should be obvious that this is a right fold. (You can
      // implement map other ways. Like a left fold composed with
      // reverse, but /this/ one is a right fold.) So why didn't I
      // just use foldr and pass in some function?

      // Because, as mentioned before, every length of list is a
      // different type, so I can't just pass in /a function/. I'd
      // need to pass in a whole family of functions. I could,
      // perhaps, pass in a whole family of functions by passing in an
      // object with a templated operator(), but even that wouldn't
      // work. A recursive invocation of foldr requires that all
      // iterations have the same return type, and each iteration of
      // map returns a sublist. So we have to code it directly.

      // To be fair, even if I could use a function object like that,
      // it would be /so much longer/ that I wouldn't want to.

      // This is a general principle, real list processing languages
      // (where all lists of type T are the same type) have no problem
      // using higher order functions that manipulate list
      // structure. Here, that's the one thing we can't really do with
      // them.
      return fun(l.head()) | map(l.tail(), fun);
    }

    template<typename T, typename Fun>
    constexpr auto map(const nil<T>&, Fun&&)
      -> nil<typename std::result_of<Fun(T)>::type> {
      return {};
    }

    // Lambdas are not LiteralType. That means they can't be used in
    // the computation of constexpr variables. You can use function
    // pointers and function objects, however. This function object
    // handles the 'capture' of the originally supplied predicate and
    // turns it into a function to be supplied to fold.
    template<typename T, typename P>
    struct count_helper {
      P pred;

      // You need to explicitly mark the constructor as constexpr if
      // you want to be able to use it to construct constexpr values.
      constexpr count_helper(P&& pred)
        : pred(std::forward<P>(pred)) {}

      // This function takes our predicate and turns it into a
      // function for accumulating a count of the number of times it's
      // true.
      constexpr std::size_t operator()(std::size_t c, const T& el) const {
        // This is called the ternary operator, if you aren't familiar
        // with it. It's a way of getting a conditional into an
        // expression rather than a statement.
        return pred(el) ? c + 1 : c;
      }
    };

    // Count itself. You could use either a left or right fold here
    // and it wouldn't matter for this function. I prefer a left fold
    // in non-lazy (that is, languages that don't evaluate expressions
    // until their results are required) languages. And they might
    // implement tail-call optimization one day.
    template<typename T, std::size_t N, typename P>
    constexpr std::size_t count(list<N, T> l, P&& p) {
      return foldl(l, 0, count_helper<T, P>(std::forward<P>(p)));
    }

    // This is another tail-recursive function. The interesting thing
    // about the tail-recursive case is that every application of the
    // function has to have the same return-type. Which, in this case,
    // means that every iteration of the function must return the
    // length of the full list we started with. That's why we our
    // return type is S + B. B is the length of the list we're
    // building and S is the length of the list we're scanning (taking
    // apart.)

    // In this case we just call ourselves again with with the head of
    // the b moved to s.
    template<std::size_t S, typename T, std::size_t B>
    constexpr list<S + B, T> reverse_helper(const list<B, T>& b,
                                            const list<S, T>& s) {
      return reverse_helper(s.head() | b, s.tail());
    }

    // And this is our base case. Once we're out of elements in our
    // source list, return the list we've built.
    template<typename T, std::size_t B>
    constexpr list<B, T> reverse_helper(const list<B, T>& b, const nil<T>&) {
      return b;
    }

    // This is just the interface to reverse, so callers don't have to
    // supply the nil.

    // Similar to what was mentioned above, this can't just be
    // implemented in terms of a left fold, whereas it could be in a
    // more conventional functional language. Even a templated
    // function object would be unable to meet the requirement of
    // having the same return type.
    template<typename T, std::size_t N>
    constexpr list<N, T> reverse(const list<N, T>& l) {
      return reverse_helper(nil<T>(), l);
    }

    // Finally, we get some function composition!

    // This is quite simple, since reverse_helper is building onto a
    // base list one by one, we just reverse the first list and supply
    // it as the 'scan' list and the second list as the 'build' list
    // to reverse_helper. The 'naive' way to do this is
    // non-tail-recursive, breaking down the first list in the least
    // specific case until it's nil, then having a slightly more
    // specific case where we break down the second list, and have our
    // termination case be the one where both are nil.
    template<typename T, std::size_t N, std::size_t M>
    constexpr list<N + M, T> append(const list<N, T>& n,
                                    const list<M, T>& m) {
      return reverse_helper(m, reverse(n));
    }

    template<std::size_t N, typename... Ts>
    constexpr list<N, std::tuple<Ts...> > zip(list<N, Ts>... ts) {
      return std::tuple<Ts...>(ts.head()...) | zip(ts.tail()...);
    }

    template<typename... Ts>
    constexpr nil<std::tuple<Ts...> > zip(list<0, Ts>... ts) {
      return {};
    }

    // There are fundamental limits on what one can do in a system
    // like this. For example, one cannot have a runtime 'pick'
    // operation. One would need to have a pick<> template, analogous
    // to the get<> function on tuples.

    // Likewise, one cannot write a filter function for this type of
    // list. The reason being that the length of a list returned by
    // filter depends entirely on the elements of the list. Thus you
    // would end up with the type of your expression dependent on
    // run-time values.

    // One of the requirements of constexpr functions is that they
    // must /also/ be valid runtime functions. Thus you can't make a
    // template whose return type is determined by 'count', even if
    // the argument to count is a constexpr list. The function would
    // be required to work just as well if you called it on a list
    // made of values the user had just typed in which is obviously
    // impossible.
  }

  using detail::list;
  using detail::nil;
  using detail::make_list;
  using detail::foldr;
  using detail::foldl;
  using detail::map;
  using detail::count;
  using detail::reverse;
  using detail::append;
  using detail::zip;
}

#endif // SCRATCH_LIST_HH
