```haskell
pure id <*> x   = x
x :: Applicative f => f a

pure (g x)      = pure g <*> pure x
g :: a -> b
x :: a

x <*> pure y    = pure (\g -> g y) <*> x
x :: f (a -> b)
y :: a

x <*> (y <*> z) = (pure (.) <*> x <*> y) <*x> z
x :: f (b -> a)
y :: f (c -> b)
z :: f c
```

