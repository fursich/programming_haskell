
```haskell
instance Functor Maybe where
-- fmap :: (a -> b) -> Maybe a -> Maybe b
fmap _ Nothing = Nothing
fmap g (Just m) = Just (g m)
```

Prove that the Functor Laws (A and B) hold for Maybe (as functor)

```haskell
fmap id      = id              -- (A)
fmap (g . h) = fmap g . fmap h -- (B)
```

We prove the above proposition by showing (A'), (B') for any x <- Maybe:

```haskell
fmap id x      = x                   -- (A')
fmap (g . h) x = (fmap g . fmap h) x -- (B')
```

Let x <- Maybe:

(i) when x = Nothing

```haskell
fmap id x      = fmap id Nothing
               = Nothing
               = x
fmap (g . h) x = fmap (g . h) Nothing
               = Nothing
               = fmap g Nothing
               = fmap g (fmap h Nothing)
               = fmap g (fmap h x)
               = (fmap g . fmap h) x
```

(ii) when x = Just m

```haskell
fmap id x      = fmap id (Just m)
               = Just (id m)
               = Just m
               = x
fmap (g . h) x = fmap (g . h) (Just m)
               = Just ((g . h) m)
               = Just (g (h m))
               = fmap g (Just (h m))
               = fmap g (fmap h (Just m))
               = fmap g (fmap h x)
               = (fmap g . fmap h) x
```
