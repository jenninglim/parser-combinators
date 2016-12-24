{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

Parser

> newtype Parser a = Parser { parse :: (String -> [(String, a)]) }

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

> newtype Fix f = In { inop :: f (Fix f) }

Our Expr AST pattern functor.

> data ExprF k = Val      Int
>              | Add      k k
>              | Sub      k k
>              | Brackets k

> item :: Parser Char
> item = Parser (\cs -> case cs of 
>                            ""     -> []
>                            (c:cs) -> [(cs,c)])
