> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> import Parser 

Type declaration for the Fix point combinator.

> newtype Fix f = In { inop :: f (Fix f) }

Type declaration for the free monad.

> data Free f a = Free (f (Free f a))
>               | Pure a

Our Expr AST pattern functor.

> data ExprF k = Val Int
>              | Add k k
>              | Sub k k
>              | Brackets k

> instance Functor f => Functor (Free f) where
>   fmap f (Pure a) = Pure $ f a
>   fmap f (Free x) = Free $ fmap (fmap f) x

> instance Functor f => Applicative (Free f) where
>   pure = Pure
>   -- (<*>) :: f (a -> b) -> f a -> f b
>   (Pure f) <*> x = fmap f x
>   (Free f) <*> x = undefined


> instance Functor f => Monad (Free f) where
>   return = Pure
>   (Free x) >>= f = Pure $ fmap (>>= f) x
>   (Pure x) >>= f = f x

> item :: Parser Char
> item = Parser (\cs -> case cs of 
>                            ""     -> []
>                            (c:cs) -> [(cs,c)])
