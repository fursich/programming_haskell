
Let Nat be
data Nat = Zero | Succ Nat

and add be
add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

## 1. Prove that `add n (Succ m) = Succ (add n m)` for any `n, m <- Nat`, using induction on n

- base case:

```haskell
  add Zero (Succ m)
= Succ m
= Succ (add Zero m)
```

- inductive step:

```haskell
  add (Succ n) (Succ m)
= Succ (add n (Succ m))
= Succ Succ (add n m)
= Succ (add (Succ n) m)
```

## 2. Prove that add n m = add m n, given the above result and `add n Zero = Zero`

We will give a proof by induction on m.

- base case:

```haskell
  add n Zero
= n
= add Zero n
```

- inductive case:

```haskell
  add n (Succ m)
= Succ (add n m)
= Succ (add m n)
= add (Succ m) Succ n
```
