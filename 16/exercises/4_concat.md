
```haskell
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
```

## Prove equation A hold for any xs
```haskell
xs ++ []         = xs                -- A
```

We give a proof by structural induction on xs

- base case:

```haskell
  [] ++ []
= []
```

- inductive step:

```haskell
  (x:xs) ++ []
= x : (xs ++ [])
= x:xs
```

## Prove equation B hold for any xs, ys, zs
```haskell
xs ++ (ys ++ zs) = (xs ++ ys) ++ zs  -- B
```

We give a proof by structural induction on xs

- base case:

```haskell
  [] ++ (ys ++ zs)
= ys ++ zs
= ([] ++ ys) ++ zs
```

- inductive step:
```haskell
  (x:xs) ++ (ys ++ zs)
= x : (xs ++ (ys ++ zs))
= x : ((xs ++ ys) ++ zs)
= (x : (xs ++ ys)) ++ zs
= ((x:xs) ++ ys) ++ zs
