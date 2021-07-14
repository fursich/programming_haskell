
```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Functor Tree where
-- fmap :: (a -> b) -> Tree a -> Tree b
fmap g (Leaf x)   = Leaf (g x)
fmap g (Node l r) = Node (fmap g l) (fmap g r)
```

Prove that the Functor Laws (A and B) hold for above Tree

```haskell
fmap id      = id              -- (A)
fmap (g . h) = fmap g . fmap h -- (B)
```

We prove the above proposition by showing (A'), (B') for any x <- Tree, using structural induction on x:

```haskell
fmap id x      = x                   -- (A')
fmap (g . h) x = (fmap g . fmap h) x -- (B')
```

Base case: (x = Leaf n)

```haskell
fmap id x      = fmap id (Leaf n)
               = Leaf (id n)
               = Leaf n
               = x
fmap (g . h) x = fmap (g . h) (Leaf n)
               = Leaf ((g . h) n)
               = Leaf (g (h n))
               = fmap g (Leaf (h n))
               = fmap g (fmap h (Leaf n))
               = fmap g (fmap h x)
               = (fmap g . fmap h) x
```

Inductive step: (x = Node l r)

```haskell
fmap id x      = fmap id (Node l r)
               = Node (fmap id l) (fmap id r)
               = Node (id l) (id r)
               = Node l r
               = x
fmap (g . h) x = fmap (g . h) (Node l r)
               = Node (fmap (g . h) l) (fmap (g . h) r)
               = Node ((fmap g . fmap h) l) ((fmap g . fmap h) r)
               = Node (fmap g (fmap h l)) (fmap g (fmap h r))
               = fmap g (Node (fmap h l) (fmap h r))
               = fmap g (fmap h (Node l r))
               = fmap g (fmap h x)
               = (fmap g . fmap h) x
```
