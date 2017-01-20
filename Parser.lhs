Parser
-----

> module Parser where

Type declaration for Parser.

> newtype Parser a = Parser { parse :: (String -> [(String, a)]) }

Functor, Applicative, Monad Instances of parser.

> instance Functor Parser where
>   fmap f (Parser p) = Parser (\cs -> [ (vs, f v) |  (vs, v) <- p cs ])

> instance Applicative Parser where
>   -- pure :: a -> f a
>   pure a = Parser (\xs -> [(xs, a)])
>   -- (<*>) :: f (a -> b) -> f a -> f b
>   (Parser fs) <*> (Parser p) = Parser (\cs -> [ (xs, f x) | (vs, f) <- fs cs, (xs, x) <- p vs])

> instance Monad Parser where
>   return = pure
>   -- (>>=) :: m a -> (a -> m b) -> m b
>   (Parser p) >>= f = Parser (\cs -> concat [ parse (f v) vs | (vs, v) <- p cs])
