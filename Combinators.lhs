> module Combinators where
> import Parser

Combinators
===========

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
> char c = satisfy (c==)

This parser has the ability to recognised strings (This is built on the 'char'
combinator). To parse the string we will first the first letter then we will 
recursively parse the remainder of the string.

> string :: String -> Parser String
> string (x:xs) = char x >>= \c -> string xs >>= \c' -> produce (c:c')
