
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
                        Nothing -> s                       -- (*1)
exec (comp' e c c') s = case eval e of
                            Just n  -> exec c (VAL n : s)
                            Nothing -> fail c' s           -- (*2)
```

## Compiler derivation (E)

* Firstly we apply each constructor of Expr from the source language into equasion (\*2).

1. with Val n

```haskell
exec  (comp' (Val n) c c') s = exec c (VAL n : s)
```

we will introduce PUSH that satisfies the following

```haskell
PUSH :: Int -> Code -> Code
exec (PUSH x c) s = exec c (VAL x : s)
```

Then the above equasion can be continued as folllows
```haskell
exec  (comp' (Val n) c c') s = exec (PUSH n c) s
```

Above equasion stands only when the following identities hold

```haskell
comp' (Val n) c c' = PUSH n c
```

2. with Throw

```haskell
exec  (comp' Throw c c') s = fail c' s
```

Define FAIL that satisfies the following

```haskell
FAIL :: Code -> Code
exec (FAIL c') s = fail c' s
```

above equasion stands when...

```haskell
comp' Throw c c' = FAIL c'
```

3. with Catch x h

```haskell
-- A4
exec (comp' (Catch x h) c c') s = case eval x of
                                      Just n  -> exec c (VAL n : s)
                                      Nothing -> case eval h of
                                          Just m  -> exec c (Val m : s)
                                          Nothing -> fail c' s
                                = case eval x of
                                      Just n  -> exec c (VAL n : s)
                                      Nothing -> exec (comp' h c c') s
```

we will define HAN <- Elem that satisfies the following

```haskell
fail c' (HAN : s) = exec c' s
```

Then A4 can be continued as follows:

```haskell
exec (comp' (Catch x h) c c') s = case eval x of
                                      Just n  -> exec c (VAL n : s)
                                      Nothing -> exec (comp' h c c') s
                                = case eval x of
                                      Just n  -> exec c (VAL n : s)
                                      Nothing -> fail (comp' h c c') (HAN : s)
```

Now define UNMARK Code <- Code, MARK Code <- Code as follows:

```haskell
exec (UNMARK c) (VAL n : HAN : s) = exec c (VAL n : s)
exec (MARK c) s = exec c (HAN : s)
```

Then A4 can further be continued as follows:

```haskell
exec (comp' (Catch x h) c c') s = case eval x of
                                      Just n  -> exec c (VAL n : s)
                                      Nothing -> fail (comp' h c c') (HAN : s)
                                = case eval x of
                                      Just n  -> exec (UNMARK c) (VAL n : HAN : s)
                                      Nothing -> fail (comp' h c c') (HAN : s)
                                = exec (comp' x (UNMARK c) (comp' h c c')) (HAN : s)
                                = exec MARK(comp' x (UNMARK c) (comp' h c c')) s
```

above equasion stands when...

```haskell
comp' (Catch x h) c c' = MARK(comp' x (UNMARK c) (comp' h c c'))
```

4. with Add x y

```haskell
-- (A3)
exec (comp' (Add x y) c c') s = case eval x of
                                    Just n -> case eval y of
                                        Just m  -> exec c (VAL (n + m) : s)
                                        Nothing -> fail c' s
                                    Nothing -> fail c' s
```

we will define ADD that satisfies the following

```haskell
ADD :: Code -> Code
exec (ADD c) (VAL m : VAL n : s) = exec c (VAL (n + m) : s)
```

also we will give additional definition for fail:
```haskell
fail c (VAL x:s) = fail c s
fail c []        = []        -- (nessesary to determine the value for the base case)
```

Then A3 can be continued as follows:
```haskell
exec (comp' (Add x y) c c') s = case eval x of
                                    Just n -> case eval y of
                                        Just m  -> exec c (VAL (n + m) : s)
                                        Nothing -> fail c' s
                                    Nothing -> fail c' s
                              = case eval x of
                                    Just n -> case eval y of
                                        Just m  -> exec (ADD c) (VAL m : VAL n : s)
                                        Nothing -> fail c' (VAL n : s)
                                    Nothing -> fail c' s
                              = case eval x of
                                    Just n -> exec (comp' y (ADD c) c') (VAL n : s)
                                    Nothing -> fail c' s
                              = exec (comp' x (comp' y (ADD c) c') c') s
```

above equasion stands when...

```haskell
comp' (Add x y) c c' = comp' x (comp' y (ADD c) c') c'
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
                      Nothing -> s
                = case eval e of
                      Just n  -> exec HALT (VAL n : s)
                      Nothing -> s
                = case eval e of
                      Just n  -> exec HALT (VAL n : s)
                      Nothing -> exec HALT s
                = eval (comp' e HALT HALT) s
```

This stands true when the following equasion holds:
```haskell
comp e = comp' e HALT HALT
```


Summarizing above calculations lead to the compiler definition below:

```haskell
type Stack = [Elem]
data Elem = VAL Int | HAN
data Code = HALT | PUSH Int Code | ADD Code | FAIL Code | MARK Code | UNMARK Code

comp :: Expr -> Code
comp e = comp' e HALT HALT

comp' :: Expr -> Code -> Code -> Code
comp' (Val n) c c'     = PUSH n c
comp' Throw c c'       = FAIL c'
comp' (Catch x h) c c' = MARK(comp' x (UNMARK c) (comp' h c c'))
comp' (Add x y) c c'   = comp' x (comp' y (ADD c) c') c'

exec :: Code -> Stack -> Stack
exec HALT s                       = s
exec (PUSH x c) s                 = exec c (VAL x : s)
exec (ADD c) (VAL m : VAL n : s)  = exec c (VAL (n + m) : s)
exec (FAIL c) s                   = fail c s
exec (UNMARK c) (VAL n : HAN : s) = exec c (VAL n : s)
exec (MARK c) s                   = exec c (HAN : s)

fail :: Code -> Stack -> Stack
fail c []         = []
fail c (VAL x:s)  = fail c s
fail c (HAN : s)  = exec c s
```

