
```haskell
instance Applicative Maybe where
-- pure :: a -> Maybe a
pure = Just

-- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
Nothing <*> _   = Nothing
(Just g) <*> mx = fmap g mx
```

Prove that the Applicative Laws hold for Maybe (as applicative)

```haskell
pure id <*> x   = x                            -- (A)
pure (g x)      = pure g <*> pure x            -- (B)
x <*> pure y    = pure (\g -> g y) <*> x       -- (C)
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z -- (D)
```

We will show identities (A), (B), (C), (D) hold for all x <- Maybe by proof 1, 2, 3, 4 respectively.

1. proposition (A)

```haskell
pure id <*> x = (Just id) <*> x
              = fmap id x
              = id x
              = x
```

2. proposition (B)

```haskell
pure (g x) = Just (g x)
           = fmap g (Just x)
           = fmap g (pure x)
           = (Just g) <*> pure x
           = pure g <*> pure x
```

3. proposition (C)

(i) when x = Nothing

```haskell
x <*> pure y = Nothing <*> pure y
             = Nothing
             = fmap (\g y -> y) Nothing
             = (Just (\g y -> y)) <*> Nothing
             = pure (\g y -> y) <*> Nothing
             = pure (\g y -> y) <*> x
```

(ii) when x = Just g

```haskell
x <*> pure y = (Just g) <*> pure y
             = fmap g (pure y)
             = fmap g (Just y)
             = Just (g y)
             = Just ((\h -> h y) g)
             = fmap (\h -> h y) (Just g)
             = (Just (\h -> h y)) <*> Just g
             = pure (\h -> h y) <*> Just g
             = pure (\g -> g y) <*> x
```

4. proposition (D)

```haskell
-- x :: Maybe (b -> c)
-- y :: Maybe (a -> b)
-- z :: Maybe a
```

(i) when x = Nothing

```haskell
x <*> (y <*> z) = Nothing <*> (y <*> z)            -- Nothing :: Maybe (b -> c)
                = Nothing                          -- Nothing :: Maybe c

(pure (.) <*> x <*> y) <*> z = (pure (.) <*> Nothing <*> y) <*> z
                             = Nothing <*> y <*> z
                             = Nothing <*> z
                             = Nothing
                             = x <*> (y <*> z)
```
(ii) when x = Just g, y = Nothing

```haskell
x <*> (y <*> z) = (Just g) <*> (y <*> z)
                = fmap g (y <*> z)
                = fmap g (Nothing <*> z)
                = fmap g Nothing
                = Nothing

(pure (.) <*> x <*> y) <*> z = (pure (.) <*> x <*> y) <*> z
                             = (Just (.) <*> x <*> y) <*> z
                             = ((fmap (.) x) <*> y) <*> z
                             = ((fmap (.) (Just g)) <*> y) <*> z
                             = ((Just ((.) g)) <*> y) <*> z
                             = (fmap ((.) g) y) <*> z
                             = (fmap ((.) g) Nothing) <*> z
                             = Nothing <*> z
                             = Nothing
                             = x <*> (y <*> z)
```

(iii) when x = Just g, y = Just h

```haskell
x <*> (y <*> z) = (Just g) <*> (y <*> z)
                = fmap g (y <*> z)
                = fmap g ((Just h) <*> z)
                = fmap g (fmap h z)
                = ((fmap g) . (fmap h)) z
                = fmap (g . h) z
                = (Just (g . h)) <*> z
                = (Just ((.) g h)) <*> z
                = (fmap ((.) g) (Just h)) <*> z
                = ((Just ((.) g)) <*> (Just h)) <*> z
                = ((fmap (.) (Just g)) <*> (Just h)) <*> z
                = ((Just (.)) <*> (Just g) <*> (Just h)) <*> z
                = (pure (.) <*> x <*> y) <*> z
```
