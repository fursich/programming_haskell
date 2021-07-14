
```haskell
type Code = [Op]
data Op = PUSH Int | ADD deriving Show

data Expr = Val Int | Add Expr Expr

comp :: Expr -> Code
comp (Val n)   = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

comp' :: Expr -> Code -> Code
comp' (Val n) c   = PUSH n : c
comp' (Add x y) c = comp' x (comp' y  (ADD : c))
```

* Prove the following identity holds for any e <- Expr, c <- Code
```haskell
comp' e c = comp e ++ c  -- (A)
```

We give a proof by struction induction on e.

Base case:

let e = Val m

```haskell
comp' (Val m) c = PUSH m : c
                = comp (Val m) ++ c
                = comp e ++ c
```

Inductive step:

let e = Add l r

```haskell
comp' (Add l r) c = comp' l (comp' r (ADD : c))
                  = comp' l (comp r ++ (ADD : c))
                  = comp l ++ comp r ++ (ADD : c)
                  = comp l ++ comp r ++ [ADD] ++ c
                  = comp (Add x y) ++ c
                  = comp e ++ c
```
