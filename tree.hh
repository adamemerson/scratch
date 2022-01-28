// -*- Mode: C++; c-basic-offset: 2; indent-tabs-mode: nil -*-*/

// scratch: Fun things to do with constexpr

#ifndef SCRATCH_TREE_HH
#define SCRATCH_TREE_HH

#include <type_traits>
#include <utility>

// Lists at compile time? That's kid stuff!

namespace scratch {
  namespace detail {

    // In the case of the list, the length of every list was encoded
    // as part of its type. In general, the /structure/ of a compile
    // time data structure is part of its type. For something like a
    // list, the entire structure is contained in just one number, its
    // length. For a tree, the shape of every tree below it has to be
    // encoded. You could represent each subtree as binary numbers or
    // something, but that's annoying and difficult. So I use a
    // recursive type.

    // This type exists just to compose two other types. We can't
    // restrict them, but we don't really have to.
    template<typename L, typename R>
    struct TT {
      using Left = L;
      using Right = R;
    };

    // And this parameterizes a node with no children. It doesn't
    // really matter what we call it, just that it be distinct from
    // all other types. In C++ every empty struct is a type of its own.
    struct LT { };

    // Since the number of template parameters varies between the
    // 'branch' and the 'leaf' case.
    template<typename ..._> class tree { };

    // And this is our tree, given structure by a tree-recursive
    // type. We combine whatever the specializing types of the left
    // and right subtrees are in the 'TT' type from above.
    template<typename L, typename R, typename T>
    class tree<TT<L, R>, T> {
    public:
      using Left = tree<L, T>;
      using Right = tree<R, T>;

    private:
      // Rather than using inheritance like we did before, we have to
      // make the subtrees actual members. If we tried to use multiple
      // inheritance, we'd have a problem if the two subtrees had the
      // same structure, thus making them the same type.
      Left l;
      Right r;

    public:

      // Our constructor just combines two subtrees
      constexpr tree(const Left& l, const Right& r)
        : l(l), r(r) {}

      // And we hand our subtrees back.
      constexpr Left left() const {
        return l;
      }
      constexpr Right right() const {
        return r;
      }
    };

    // Notice the leaf, it has no subtrees at all, and is specialized
    // with LT rather than the TT template.
    template<typename T>
    class tree<LT, T> {
      T v;

    public:
      constexpr tree(T v) : v(v) {}

      constexpr T value() const& {
        return v;
      }
    };

    template<typename T>
    using leaf = tree<LT, T>;

    // A similar 'cons' operator. I used this one only because it
    // makes me think of the slash lines one would use to draw ASCII
    // art tree diagrams.
    template<typename L, typename R, typename T>
    constexpr auto operator ^(const tree<L, T>& l, const tree<R, T>& r)
      -> tree<TT<L, R>, T> {
      return {l, r};
    }

    // Just a simple convenience wrapper to infer the type.
    template<typename T>
    constexpr leaf<T> make_leaf(T v) {
      return {v};
    }

    // Yes, yes, I didn't call it fold. And it's not a fold,
    // unfortunately. Because every subtree is a different type,
    // there's no way to take a function that is analogous to anything
    // but the leaf constructor. We can traverse the values in the
    // leaves, but we're limited enough in our use of the structure
    // that we may as well not even have a tree. We could perhaps do
    // things like call a function with the leaf count or the depth of
    // the longest path under every given node, but the appeal of the
    // method is starting to wane.
    template<typename S, typename T, typename F>
    constexpr T reduce_leaves(const tree<S, T> t, F&& f) {
      return f(reduce_leaves(t.left(), std::forward<F>(f)),
               reduce_leaves(t.right(), std::forward<F>(f)));
    }

    // And the base case.
    template<typename T, typename F>
    constexpr T reduce_leaves(const leaf<T> t, F&&) {
      return t.value();
    }

    // Since we were using an integer to encode the structure of our
    // list, finding the length of a list was trivial. For trees,
    // where we have a recursive type encoding the structure, that
    // isn't the case.

    // We could use a recursive function that returns f(left) +
    // f(right) and bottoms out with f(leaf) = 1. But in that case the
    // type system is doing basically all the work. There's no point
    // in having a function.

    // We can just create a recursive type identical to the number of
    // leaves in a tree.
    template<typename S, typename T>
    struct count_leaves : public std::integral_constant<
      std::size_t,
      count_leaves<typename S::Left, T>::value +
      count_leaves<typename S::Right, T>::value> {};

    template<typename T>
    struct count_leaves<leaf<T>, T>: public std::integral_constant<
      std::size_t, 1> {};

    // It has a similar structure to all the recursive list functions
    // we defined before. And it makes a point. If any manipulation or
    // operation on the structure of a tree or list takes place purely
    // in the type system, and I can make something like the count of
    // leaves in a tree be a type, maybe the correct solution for
    // compile-time data structures is to abandon functions altogether.
  }

  using detail::tree;
  using detail::leaf;
  using detail::make_leaf;
  using detail::reduce_leaves;
  using detail::count_leaves;
}

#endif // SCRATCH_TREE_HH
