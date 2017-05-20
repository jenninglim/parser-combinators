> module Combinators where
> import Parser

Combinators
===========

Combinators have two definitions:

 * A function or defintion with no free variables - a variable that is not bound.

 * A style of organising libraries centered around the idea of combining. This
   builds a complex set of patterns from small set of simple primitives.

> (<$>) :: Functor f => (a -> b) -> f a -> f b
> f <$> g = fmap f g

This is an infix fmap.

> (<$) :: Functor f => a -> f b -> f a
> (<$) = fmap . const

This will first apply the `const` (a -> b -> a) to the first argument. This function intuitively
takes two argument and simply ignore the results from the second and returns the
first result. Thus, const can be partially applied to produce a function of type
(b -> a) which can be lifted using fmap.

This combinator is used to "ignore" the results from the leftmost functor.
This works by replacing all locations in the input with the same value.

< (<*>) :: Applicative f => f (a -> b) -> f a -> f b

> (<*) :: Applicative f => f a -> f b -> f a
> (<*) = liftA2 const
>    where liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
>          liftA2 f a b = fmap f a <*> b

'liftA2' is a function that lifts a function with two arguments such that it
can be applied to applicatives. This is very similar to (<*>) except with
two arguments.

Note: Function application has higher precedence so fmap f a <*> b == (fmap f a) <*> b.

This combinator is used to sequence actions while ignore the results from the
leftmost applicative.

> (*>) :: Applicative f => f a -> f b -> f b

Recall:

> (<|>) :: Alternative f => f a -> f a -> f a
> (<|>) = undefined

> many :: Alternative f => f a -> f [a]
> many = undefined

> some :: Alternative f => f a -> f [a]
> some = undefined

> sepBy :: Alternative f => f a -> f sep -> f a 
> sepBy = undefined
