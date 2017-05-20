> module Parser where

Parser 
======

Type Definition
---------------

> data Parser a = Parser (String -> [(a , String)])

A function that takes a string and returns all the combination of possible parses
and its corresponding remaining unparsed string.

Functor Instance
----------------

Recall:

< class Functor f where
<   fmap :: (a -> b) -> f a -> f b

> instance Functor Parser where
>   fmap f (Parser p) = Parser (\ts -> [ (f x, ts') | (x, ts') <- p ts])

This can be thought as lifting the function (a -> b) to (f a -> f b). Thus this will
transform the parser such that the function is applied to the results of the parser.
i.e it will lift some function a -> b by transforming its results.

Applicative Instance
--------------------

Recall:

< class Applicative f where
<    pure  :: a -> f a
<    (<*>) :: f (a -> b) -> f a -> f b

Applicatives are useful for sequencing effects.

We have for the parser instance:

> instance Applicative Parser where
>   pure x                    = Parser (\ts -> [(x, ts)])
>   (Parser f) <*> (Parser p) = Parser (\ts -> [(g x, ts'') | (g, ts') <- f ts, (x, ts'') <- p ts'])

"pure" will be equivalent to "produce" - seen in the lectures (Lecture 9).

> produce :: a -> Parser a
> produce x = Parser (produce' x)

> produce' :: a -> String -> [(a, String)]
> produce' x ts = [(x, ts)]

Note produce = pure. 
It is a parser that does not consume any input.
This can also be done using annonymous functions.

However, for applicative instance (f <*> g)  intiutively we have a parser f that returns a function. When applied
applicatively to g, the result from applying f will be a list of tuples in the form (function, remaining string). The 
remaining string is then applied to the parser g and then the result is transformed by the function.

> runParser :: Parser a -> String -> [(a, String)]
> runParser (Parser p) = p 

Alternative Instance 
--------------------

Recall:

> class Applicative f => Alternative f where
>   -- | This is the identity of <|>
>   empty :: f a 
>   -- | An associative binary operation.
>   (<|>) :: f a -> f a -> f a

Alternatives is a subclass of applicatives and the above functions MUST
be defined as a minimum. Alternatives must satisfy the following laws:

 * empty <|> q = q
 * q <|> empty = q

Note: "empty" is an applicative compution with no results. 
      (<|>) is used to combine two computations.

>   -- | Zero or more.
>   many :: f a -> f [a]
>   many v = some v <|> pure []

>   -- | One or more.
>   some :: f a -> f [a]
>   some v = (:) <$> v <*> many v

Notice that 'many' and 'some' are mutually recursive.
Intuitively, since many is zero or more, we have pure [] to capture the idea of zero,
then one or more will be captured by some. This is equivalent to the BNF
operation *.

In the definition of some, we have one or more. This is equivalent to
having one occuring followed by zero or more. Thus some will be defined
in terms of many and is the same as first sequencing some action
followed by many of that action with its results combined using the (<*>).
This is equivalent to the BNF operation +.

List is an alternative instance also:

> instance Alternative [] where
>   empty = []
>   (<|>) = (++)

And so is a parser!

> instance Alternative Parser where
>   empty = Parser $ \_ -> []
>   (Parser p) <|> (Parser q) = Parser $ \ts -> case p ts of
>                                                 [] -> q ts
>                                                 xs -> xs
>   many v = some v <|> produce []
>   some v = (:) <$> v <*> many v

The identity of (<|>) is equivalent to the failure parser.

The (<|>) definition is more interesting. This definition implies
that given an input string we will apply the first parser
then if p fails without consuing any input, parser q is tried.

Monad Instance
--------------

Recall:

< class Monad m where
<   return :: a -> m a
<   (>>=) :: m a -> (a -> m b) -> m b

Monads deal with sequencing actions.
(This is a simplification and is enough for this course)

Monads must satisfy several laws:

 * return x >>= f = f x -- Right identity (or unit).
 * m x >>= return = m x -- Left identity (or unit).
 * (m x >>= f) >>= g == m x >>= (\x -> f x >>= g) -- Associativity.
> instance Monad Parser where
>   return = pure
>   (Parser p) >>= f = Parser (\ts -> concat [ runParser (f x) ts' | (x, ts') <- p ts ])

(>>=) is interesting. The idea is that we will apply p and sequence another parse with f,
this is what is captured in the defintion. After applying our parser p, we will use the
result (from parser p) to sequence the next parser by applying f.
