
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
                            Nothing -> exec c' s           -- (*2)
```

## Compiler derivation (D)

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
exec  (comp' Throw c c') s = exec c' s
```

above equasion stands when...

```haskell
comp' Throw c c' = c'
```

3. with Catch x h

```haskell
-- A4
exec (comp' (Catch x h) c c') s = case eval x of
                                      Just n  -> exec c (VAL n : s)
                                      Nothing -> case eval h of
                                          Just m  -> exec c (Val m : s)
                                          Nothing -> exec c' s
                                = case eval x of
                                      Just n  -> exec c (VAL n : s)
                                      Nothing -> exec (comp' h c c') s
                                = exec (comp' x c (comp' h c c')) s
```

above equasion stands when...

```haskell
comp' (Catch x h) c c' = comp' x c (comp' h c c')
```

4. with Add x y

```haskell
-- (A3)
exec (comp' (Add x y) c c') s = case eval x of
                                    Just n -> case eval y of
                                        Just m  -> exec c (VAL (n + m) : s)
                                        Nothing -> exec c' s
                                    Nothing -> exec c' s
```

we will define ADD that satisfies the following

```haskell
ADD :: Code -> Code
exec (ADD c) (VAL m : VAL n : s) = exec c (VAL (n + m) : s)
```

also define POP as follows

```haskell
POP :: Code -> Code
exec (POP c) (VAL _ :s) = exec c s
```

A3 can be continued as follows:
```haskell
exec (comp' (Add x y) c c') s = case eval x of
                                    Just n -> case eval y of
                                        Just m  -> exec c (VAL (n + m) : s)
                                        Nothing -> exec c' s
                                    Nothing -> exec c' s
                              = case eval x of
                                    Just n -> case eval y of
                                        Just m  -> exec (ADD c) (VAL m : VAL n : s)
                                        Nothing -> exec c' s
                                    Nothing -> exec c' s
                              = case eval x of
                                    Just n -> case eval y of
                                        Just m  -> exec (ADD c) (VAL m : VAL n : s)
                                        Nothing -> exec (POP c') (VAL n : s)
                                    Nothing -> exec c' s
                              = case eval x of
                                    Just n -> exec (comp' y (ADD c) (POP c') (Val n : s)
                                    Nothing -> exec c' s
                              = exec (comp' x (comp' y (ADD c) (POP c')) c') s
```

above equasion stands when...

```haskell
comp' (Add x y) c c' = comp' x (comp' y (ADD c) (POP c')) c'
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
data Elem = VAL Int
data Code = HALT | PUSH Int Code | ADD Code | POP Code

comp :: Expr -> Code
comp e = comp' e HALT HALT

comp' :: Expr -> Code -> Code -> Code
comp' (Val n) c c'     = PUSH n c
comp' (Throw c c') s   = c'
comp' (Add x y) c c'   = comp' x (comp' y (ADD c) (POP c')) c'
comp' (Catch x h) c c' = comp' x c (comp' h c c')

exec :: Code -> Stack -> Stack
exec HALT s                      = s
exec (PUSH x c) s                = exec c (VAL x : s)
exec (ADD c) (VAL m : VAL n : s) = exec c (VAL (n + m) : s)
exec (POP c) (VAL _ :s)          = exec c s
```

