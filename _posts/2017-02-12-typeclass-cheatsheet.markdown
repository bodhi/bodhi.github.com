---
title: A cheat-sheet for Haskell & PureScript typeclasses
layout: default
---

One-page overview of a bunch of different Haskell & PureScript
typeclasses. Starting with the
[PureScript Class Hierarchy diagram](https://pursuit.purescript.org/packages/purescript-control/2.0.0),
and similar on the
[Haskell Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Introduction). Very
much a work in progress: info and terminology may be completely wrong!

---

## Functor

Map a value inside a context to a new value:

```
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

## Applicative

With a function (or, more intuitively, a partial computation) inside a
context, map a value also inside the same context:

```
class Functor f => Applicative f where
  (<*>) :: f (a -> b) -> f a -> f b
```


Also provide a way to lift a value into a context:

```
  pure  :: a -> f a
```

### Usage

> For instance, how could we sum `Just 2` and `Just 3`? [^haskellbook-applicative]

```
> :t fmap (+)
fmap (+) :: (Functor f, Num a) => f a -> f (a -> a)
> :t fmap (+) (Just 2)
fmap (+) (Just 2) :: Num a => Maybe (a -> a)
```

Ah, now we can use `<*>`:

```
fmap (+) (Just 2) <*> (Just 3)
```

So when you have a function `a -> b -> c`, partially apply it, and
lift into the *functor*, you end up with `f (b -> c)`, then to apply
it to a second parameter of type `b`, you use `<*>`, and end up with
`f c`. Thus:

> `<*>` is just function application within a computational
> context.[^typeclassopedia-applicative]

[^typeclassopedia-applicative]: [Typeclassopedia: §4.1 \[Applicative\] Definition](https://wiki.haskell.org/Typeclassopedia#Definition_2)
[^haskellbook-applicative]: [Haskell/Applicative functors on WikiBooks](https://en.wikibooks.org/wiki/Haskell/Applicative_functors)

## Monad

Use the result of a previous *computation* as input into another
computation, i.e. *sequence* computations.

One consequence is that a sequence may not preserve the structure of
an input. An example: when under `Maybe`, *functors* and
*applicatives* can't change the result from `Just a` to `Nothing`,
since none of the parameters return `f b`, they only return `b`. You
need to "upgrade" to a *monad* to be able to use a function like `a ->
m b`.

<aside>
Not 100% sure if "preserve the structure" is correct terminology.
</aside>

---

## Semigroup

Set `S` with a binary operation `<>` that *combines* two elements of
`S` into another element of `S`:

```
class Semigroup a where
  (<>) :: a -> a -> a
```

`<>` must be associative.

## Monoid

Extension of a *Semigroup* with an identity element `i`, such that `s
<> i == i <> s == s`.

## Semigroupoid

Set of objects with composable morphisms. E.g. like a *category*, but
without *requiring* an identity element.

```
class Semigroupoid a where
  compose :: forall b c d. a c d -> a b c -> a b d
```

`(->)` is a *semigroupoid*, with `(.)` as `compose`: `(.) :: (b -> c) ->
(a -> b) -> (a -> c)`.


## Category

*Semigroupoid* with an identity element.

To be less facetious, a category is a set of objects, composable
morphisms, and an identity element, such that `id <<< p = p <<< id = p`

## Group

A *Group* is a *Monoid* with inverses. Numbers form a group with `+`
as the inverse of a value `a` is `-a`:

```
a + -a = zero
```

## Semiring

types that support addition and multiplication.

## Ring

Semiring that also supports subtraction.

## Arrow

> a value of type ``b `arr` c`` can be thought of as a computation
> which takes values of type `b` as input, and produces values of type
> `c` as output.
