

```haskell
take 0 _      = []
take _ []     = []
take n (x:xs) = x : take (n-1) xs

drop 0 xs     = xs
drop _ []     = []
drop n (_:xs) = drop (n-1) xs
```

## Prove `take n xs ++ drop n xs = xs` for any n >= 0

We give a proof by structural induction on n and xs

- base case (for n):

```haskell
  take 0 xs ++ drop 0 xs
= [] ++ xs
= ns
```

- base case (for xs):

```haskell
  take n [] ++ drop n []
= [] ++ []
= []
```

- inductive step:

```haskell
  take n (x:xs) ++ drop n (x:xs)
= (x: take (n-1) xs) ++ drop (n-1) xs
= x: (take (n-1) xs ++ drop (n-1) xs)
= x: xs
```
