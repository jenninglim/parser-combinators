> module Parser where

Parser and its intuition
======================== 

Definition
----------

> data Parser a = Parser (String -> [(a , String)])

A function that takes a string and returns all the combination of possible parsers
and its remaining unparsed string.

Functor
-------

> instance Functor Parser where
>   fmap f (Parser p) = Parser (\ts -> [ (f x, ts') | (x, ts') <- p ts])

We want to apply the parser on a string and the apply our function f into our parser.
i.e it will lift some function a -> b into our parser by transofrming its resultant
parses.

Applicative
-----------

> instance Applicative Parser where
>   pure x                    = Parser (\ts -> [(x, ts)])
>   (Parser f) <*> (Parser p) = Parser (\ts -> [(g x, ts'') | (g, ts') <- f ts, (x, ts'') <- p ts'])

The pure will be equivalent to "produce" in the lectures (Lecture 9).

> produce :: a -> Parser a
> produce x = Parser (produce' x)

> produce' :: a -> String -> [(a, String)]
> produce' x ts = [(x, ts)]

Note produce = pure. This can be done using annonymous functions.
It is a parser that does not consume any input.

However, for applicative instance (f <*> g)  intutively we have a parser f that returns a function. When applied
applicatively to g, the result from f will be a list of tuples in the form (function, remaining string). The 
remaining string is then applied to the parser g and then the result is transformed by the function.

> runParser :: Parser a -> String -> [(a, String)]
> runParser (Parser p) = p 

Monad
-----

Monads deal with sequenticing operations in functional languages. (This is a simplification and is enough for this course)

> instance Monad Parser where
>   return = pure
>   (Parser p) >>= f = Parser (\ts -> concat [ runParser (f x) ts' | (x, ts') <- p ts ])

(>>=) is interesting. The idea is that we will apply p and sequence another parse with f, this is what is 
captured in the defintion. After applying our parser p, we will use the the result to then sequence the next parser.
