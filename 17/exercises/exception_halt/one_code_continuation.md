
## Definition
* souce language syntax
```haskell
data Expr = Val Int | Add Expr Expr | Throw | Catch Expr Expr
```

* source language semantics -- (D1)
```haskell
eval :: Expr -> Maybe Int
eval (Val n)     = Just n
eval (Add x y)   = case eval x of
                       Just n -> case eval y of
                           Just m -> Just (n + m)
                           Nothing -> Nothing
                       Nothing -> Nothing
eval Throw       = Nothing
eval (Catch x h) = case eval x of
                       Just n -> Just n
                       Nothing -> eval h
```

## Compiler specification

Let us assume that Stack can only store VAL n (realistic assumption compared to "nullabule stack" approach)

```haskell
type Stack = [Elem]
data Elem = VAL Int
```

Now let us assume that there exists a set of language, compiers and virtual machines - i.e. (Code, comp, comp', exec) - that satisfies the following equasions.

```haskell
eval :: Expr -> Maybe Int
exec (comp e) s    = case eval e of
                        Just n  -> VAL n : s
                        Nothing -> fail s            -- (*1)
exec (comp' e c) s = case eval e of
                        Just n -> exec c (VAL n : s)
                        Nothing -> fail s            -- (*2)
```

## Compiler derivation (C)

* Firstly we apply each constructor of Expr from the source language into equasion (\*2).

1. with Val n

```haskell
exec  (comp' (Val n) c) s = exec c (VAL n : s)
```

we will introduce PUSH that satisfies the following

```haskell
PUSH :: Int -> Code -> Code
exec (PUSH x c) s = exec c (VAL x : s)
```

Above equasion stands only when the following identities hold

```haskell
comp' (Val n) c = PUSH n c
```

2. with Throw

```haskell
exec  (comp' Throw c) s   = fail s
```

Define FAIL that satisfies the following

```haskell
FAIL :: Code
exec FAIL s = fail s
```
Above equasion stands only when the following identities hold

```haskell
comp' Throw c = FAIL
```

3. with Add x y

```haskell
-- (A3)
exec (comp' (Add x y) c) s = case eval x of
                                 Just n -> case eval y of
                                     Just m -> exec c (VAL (n + m) : s)
                                     Nothing -> fail s
                                 Nothing -> fail s
```

we will define ADD that satisfies the following

```haskell
ADD :: Code -> Code
exec (ADD c) (VAL m : VAL n : s) = exec c (VAL (n + m) : s)
```

also we will give additional definition for fail:
```haskell
fail (VAL x:s) = fail s
fail [] = []        -- (nessesary to determine the value for the base case)
```

Then A3 can be continued as follows:
```haskell
exec (comp' (Add x y) c) s = case eval x of
                                 Just n -> case eval y of
                                     Just m -> exec c (VAL (n + m) : s)
                                     Nothing -> fail s
                                 Nothing -> fail s
                           = case eval x of
                                 Just n -> case eval y of
                                     Just m -> exec (ADD c) (VAL m : VAL n : s)
                                     Nothing -> fail (VAL n : s)
                                 Nothing -> fail s
                           = case eval x of
                                 Just n -> exec (comp' y (ADD c)) (VAL n : s)
                                 Nothing -> fail s
                           = exec (comp' x (comp' y (ADD c))) s
```

above equasion stands when...

```haskell
comp' (Add x y) c = comp' x (comp' y (ADD c))
```

4. with Catch x h

```haskell
-- A4
exec (comp' (Catch x h) c) s = exec c (eval (Catch x h) : s)
                             = case eval x of
                                   Just n  -> exec c (VAL n : s)
                                   Nothing -> case eval h of
                                       Just m -> exec c (VAL m: s)
                                       Nothing -> fail s
                             = case eval x of
                                   Just n  -> exec c (VAL n : s)
                                   Nothing -> exec (comp' h c) s
```

we will define HAN Code <- Elem that satisfies the following

```haskell
fail (HAN c' : s) = exec c' s
```

Then A4 can be continued as follows:

```haskell
exec (comp' (Catch x h) c) s = case eval x of
                                   Just n  -> exec c (VAL n : s)
                                   Nothing -> exec (comp' h c) s
                             = case eval x of
                                   Just n  -> exec c (VAL n : s)
                                   Nothing -> fail (HAN (comp' h c) : s)
```

Now define UNMARK Code <- Code, MARK Code Code <- Code as follows:

```haskell
exec (UNMARK c) (VAL n : HAN _ : s) = exec c (VAL n : s)
exec (MARK c' c) s = exec c (HAN c' : s)
```

Then A4 can further be continued as follows:

```haskell
exec (comp' (Catch x h) c) s = case eval x of
                                   Just n  -> exec c (VAL n : s)
                                   Nothing -> fail (HAN (comp' h c) : s)
                             = case eval x of
                                   Just n  -> exec (UNMARK c) (VAL n : HAN (comp' h c) : s)
                                   Nothing -> fail (HAN (comp' h c) : s)
                             = exec (comp' x (UNMARK c)) (HAN (comp' h c) : s)
                             = exec (MARK (comp' h c) (comp' x (UNMARK c)) s
```

above equasion stands when

```haskell
comp' (Catch x h) c = MARK (comp' h c) (comp' x (UNMARK c))
```

* Secondly we apply e <- Expr to equasion (\*1).

```haskell
exec (comp e) s = eval e : s
```

we will define HALT so that..
```haskell
HALT :: Code
exec HALT s = s
```

Using (\*2) we will have..
```haskell
eval (comp e) s = case eval e of
                      Just n  -> VAL n : s
                      Nothing -> fail s
                = case eval e of
                      Just n  -> exec HALT (VAL n : s)
                      Nothing -> fail s
                = eval (comp' e HALT) s
```

This stands true when the following equasion holds:
```haskell
comp e = comp' e HALT
```


Summarizing above calculations lead to the compiler definition below:

```haskell
type Stack = [Elem]
data Elem = VAL Int | HAN Code
data Code = HALT | PUSH Int Code | ADD Code | FAIL | MARK Code Code | UNMARK Code

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c     = PUSH n c
comp' Throw c       = FAIL
comp' (Add x y) c   = comp' x (comp' y (ADD c))
comp' (Catch x h) c = MARK (comp' h c) (comp' x (UNMARK c))

exec :: Code -> Stack -> Stack
exec HALT s                         = s
exec (PUSH x c) s                   = exec c (VAL x : s)
exec (ADD c) (VAL m : VAL n : s)    = exec c (VAL (n + m) : s)
exec FAIL s                         = fail s
exec (MARK c' c) s                  = exec c (HAN c' : s)
exec (UNMARK c) (VAL n : HAN _ : s) = exec c (VAL n : s)

fail :: Stack -> Stack
fail [] = []
fail (VAL _ : s) = s
fail (HAN c : s) = exec c s
```

