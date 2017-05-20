> module Parser where

Parser 
======

Type Definition
---------------

> data Parser a = Parser (String -> [(a , String)])

A function that takes a string and returns all the combination of possible parsers
and its remaining unparsed string.

Functor Instance
----------------

Recall:

< class Functor f where
<   fmap :: (a -> b) -> f a -> f b

> instance Functor Parser where
>   fmap f (Parser p) = Parser (\ts -> [ (f x, ts') | (x, ts') <- p ts])

We want to apply the parser on a string and the apply our function f into our parser.
i.e it will lift some function a -> b into our parser by transofrming its resultant
parses.

Applicative Instance
--------------------

Recall:

< class Applicative f where
<    pure :: a -> f a
<    (<*>) :: (a -> b) -> f a -> f b

Applicatives are useful for sequencing effects.

We have for the parser instance.

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

Monad Instance
--------------

Recall:

< class Monad m where
<   return :: a -> m a
<   (>>=) :: m a -> (a -> m b) -> m b

Monads deal with sequencing actions.
(This is a simplification and is enough for this course)

> instance Monad Parser where
>   return = pure
>   (Parser p) >>= f = Parser (\ts -> concat [ runParser (f x) ts' | (x, ts') <- p ts ])

(>>=) is interesting. The idea is that we will apply p and sequence another parse with f, this is what is 
captured in the defintion. After applying our parser p, we will use the the result to then sequence the next parser.


Parsers
=======

A parser that fails.

> failure :: Parser a
> failure = Parser (\_ -> [])

A simple parser that returns the next item in the input stream.

> item :: Parser Char
> item = Parser (\ts -> case ts of 
>                         []     -> []
>                         (x:xs) -> [(x, xs)])

This parser checks if the first character satisfies some predicate.

> satisfy :: (Char -> Bool) -> Parser Char
> satisfy p = item >>= \x -> if p x
>                               then produce x 
>                               else failure

This will return a single char if it satisfies a predicate.

> char :: Char -> Parser Char
> char c = satisfy (c ==)

This parser has the ability to recognised strings (This is built on the 'char'
combinator). To parse the string we will first the first letter then we will 
recursively parse the remainder of the string.

> string :: String -> Parser String
> string (x:xs) = char x >>= \c -> string xs >>= \c' -> produce (c:c')
