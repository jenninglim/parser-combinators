{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

Parser

> newtype Parser a = String -> [(String, a)]

> newtype Fix f = In { inop :: f (Fix f) }

Our Expr AST pattern functor.

> data ExprF k = Val      Int
>              | Add      k k
>              | Sub      k k
>              | Brackets k

> type Expr = Fix ExprF
