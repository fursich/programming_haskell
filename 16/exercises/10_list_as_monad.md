
```haskell
instance Functor [] where
fmap g [] = []
fmap g (x:xs) = (g x) : (fmap g xs)

instance Applicative [] where
-- pure :: a -> [a]
pure x = [x]

-- (<*>) :: [a -> b] -> [a] -> [b]
gs <*> xs = [g x| g <- gs, x <- xs]

instance Monad [] where
return = pure
-- (>>=) :: [a] -> (a -> [b]) -> [b]
xs >>= f = [y | x <- xs, y <- f x]

```

Prove that the Applicative Laws hold for Maybe (as applicative)

```haskell
return x >>= f   = f x                        -- (A)
mx >>= return    = mx                         -- (B)
(mx >>= f) >>= g = mx >>= (\x -> (f x >>= g)) -- (C)
```

We will show identities (A), (B), (C) hold for all x <- [a] by proof 1, 2, 3 respectively.

1. proposition (A)

```haskell
return x >>= f = pure x >>= f
               = [x] >>= f
               = [y | x' <- [x], y <- f x']
               = [y | y <- f x]
               = f x
```

2. proposition (B)

```haskell
mx >>= return = [y | x <- mx, y <- return x]
              = [y | x <- mx, y <- pure x]
              = [y | x <- mx, y <- [x]]
              = [y | y <- mx]
              = mx
```

3. proposition (C)

(Lemma)
First, we prove that identities (P), (Q), (R) stand for `>>= :: [a] -> (a -> [b]) -> [b]`

```haskell
[] >>= f     = []                           -- (P)
(x:xs) >>= f = (f x) ++ (xs >>= f)          -- (Q)
(xs ++ ys) >>= f = (xs >>= f) ++ (ys >>= f) -- (R)
```

(P)

```haskell
[] >>= f = [y | x <- [], y <- f x]
         = []
```

(Q)

```haskell
(x:xs) >>= f = [y | x' <- (x:xs), y <- f x']
             = [y | y <- f x] ++ [y | x' <- xs, y <- f x']
             = (f x) ++ (xs >>= f)
```

(R)

```haskell
(xs ++ ys) >>= f = [y | x' <- (xs ++ ys), y <- f x']
                 = [y | x' <- xs, y <- f x'] ++ [y | x' <- ys, y <- f x']
                 = (xs >>= f) ++ (ys >>= f)


(Proof)

```haskell
(mx >>= f) >>= g = mx >>= (\x -> (f x >>= g)) -- (C)
```

We will give a proof on structural induction on mx <- [a].

(i) Base case:
when mx = []

```haskell
(mx >>= f) >>= g = ([] >>= f) >>= g
                 = [] >>= g
                 = []
                 = [] >>= (\x -> (f x >>= g))
                 = mx >>= (\x -> (f x >>= g))
```

(ii) Inductive step:
when mx = (x: xs), we assume that (C) holds for [x] and xs.

```haskell
(mx >>= f) >>= g = ((x:xs) >>= f) >>= g
                 = (([x] ++ xs) >>= f) >>= g
                 = (([x] >>= f) ++ (xs >>= f)) >>= g
                 = (([x] >>= f) >>= g) ++ ((xs >>= f) >= g)
                 = ([x] >>= (\x -> (f x >>= g))) ++ (xs >>= (\x -> (f x >>= g))
                 = ([x] ++ xs) >>= (\x -> (f x >>= g))
                 = (x:xs) >>= (\x -> (f x >>= g))
                 = mx >>= (\x -> (f x >>= g))
```
