
```haskell
all p []     = True
all p (x:xs) = p x && all p xs

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x
```

## Prove that `all (== x) (replicate n x) = True` holds for any x >= 0

We give a proof by induction on n.

- base case:
```haskell
  all (== x) (replicate 0 x)
= all (== x) []
= True
```

- inductive step:

for any n > 0,

```haskell
  all (== x) (replicate n x)
= all (== x) (x: replicate (n-1) x)
= (== x) x && replicate (n-1) x
= True && all (== x) replicate (n-1) x
= all (== x) replicate (n-1) x
= True
```
