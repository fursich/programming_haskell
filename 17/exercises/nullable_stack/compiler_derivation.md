
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

* for the sake of simplicity, let us define two functions that operates on Maybe a

```haskell
(+) :: Maybe Int -> Maybe Int -> Maybe Int
Just x + Just y = Just (x Prelude.+ y)
Nothing + _     = Nothing
_ + Nothing     = Nothing

(||) :: Maybe Int -> Maybe Int -> Maybe Int
Just x || _       = Just x
Nothing || Just y = Just y
```

* using above operators, source language semantics (D1) can be rewritten as follows -- (D2)

```haskell
eval :: Expr -> Maybe Int
eval (Val n)     = Just n
eval Throw       = Nothing
eval (Add x y)   = eval x + eval y
eval (Catch x h) = eval x || eval h
```


## Compiler specification

Now let us assume that there exists a set of language, compiers and virtual machines - i.e. (Code, comp, comp', exec) - that satisfies the following equasions.

```haskell
eval :: Expr -> Maybe Int
exec (comp e) s    = eval e : s          -- (*1)
exec (comp' e c) s = exec c (eval e : s) -- (*2)
```

Let us also assume here that `Stack` can store "Nothing" as well as Val n for the sake of simplicity, albeit not really be a realistic assumption.

```haskell
type Stack = [Maybe Int]
```

## Compiler derivation (A)

* Firstly we apply each constructor of Expr from the source language into equasion (\*2).

1. with Val n, and Throw

```haskell
exec  (comp' (Val n) c) s = exec c (eval (Val n) : s)
                          = exec c (Just n : s)
exec  (comp' Throw c) s   = exec c (eval Throw : s)
                          = exec c (Nothing : s)
```

we will introduce PUSH that satisfies the following

```haskell
PUSH :: Maybe Int -> Code -> Code
exec (PUSH x c) s = exec c (x : s)
```

Above equasion stands only when the following identities hold

```haskell
comp' (Val n) c = PUSH (Just n) c
comp' Throw   c = PUSH Nothing c
```

2. with Add x y

```haskell
exec (comp' (Add x y) c) s = exec c (eval (Add x y) : s)
                           = exec c ((eval x + eval y) : s)
```

we will define ADD that satisfies the following

```haskell
ADD :: Code -> Code
exec (ADD c) (m : n : s) = exec c (n + m : s)
```

above equasion stands when...

```haskell
comp' (Add x y) c = comp' x (comp' y (ADD c))
```

3. with Catch x h

```haskell
exec (comp' (Catch x h) c) s = exec c (eval (Catch x h) : s)
                             = exec c ((eval x || eval h) : s)
```

we will define CATCH that satisfies the following
```haskell
CATCH :: Code -> Code
exec (CATCH c) (m : n : s) = exec c (n || m : s)
```

above equasion stands when
```haskell
comp' (Catch x h) c = comp' x (comp' h (CATCH c))
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
eval (comp e) s = eval e : s
                = exec HALT (eval e : s)
                = exec (comp' e HALT) s
```

This stands true when the following equasion holds:
```haskell
comp e = comp' e HALT
```

Summarizing above calculations lead to the compiler definition below:

```haskell
data Code = HALT | PUSH (Maybe Int) Code | ADD Code | CATCH Code

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH (Just n) c
comp' Throw c   = PUSH Nothing c
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' (Catch x h) c = comp' x (comp' h (CATCH c))

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH x c) s = exec c (x : s)
exec (CATCH c) (m : n : s) = exec c (n || m : s)
exec (ADD c) (m : n : s) = exec c (n + m : s)
```

## Another compiler derivation (B)

The first approach is costly when executing CATCH, as it requires the virtual machine to always evaluate both x and h regardless the former evaluation.
We consider to avoid this situation using a different type of compiler.

In above calculations (3), we could use another interpretation for CATCH.

3'. with Catch x h

```haskell
exec (comp' (Catch x h) c) s = exec c (eval (Catch x h) : s)
                             = case eval x of
                                   Just n  -> exec c ((eval (Just n) : s)
                                   Nothing -> exec c (eval h : s)  -- = exec c (exec (comp h) s)
```

we will define CATCH that satisfies the following

```haskell
CATCH :: Code -> Code -> Code
exec (CATCH c' c) (m : s) = case m of
                                Just n  -> exec c (m : s)
                                Nothing -> exec c (exec c' s)
```

above equasion stands when

```haskell
comp' (Catch x h) c = comp' x (CATCH (comp h) c)
```

Summarizing above calculations lead to the compiler definition below:

```haskell
data Code = HALT | PUSH (Maybe Int) Code | ADD Code | CATCH Code Code

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH (Just n) c
comp' Throw c   = PUSH Nothing c
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' (Catch x h) c = comp' x (CATCH (comp h) c)

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH x c) s = exec c (x : s)
exec (CATCH c' c) (m : s) = case m of
                                Just n  -> exec c (m : s)
                                Nothing -> exec c (exec c' s)
exec (ADD c) (m : n : s) = exec c (n + m : s)
```

