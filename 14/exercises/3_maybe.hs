import Data.Foldable
import Data.Monoid

data Maybe' a = Just' a | Nothing' deriving Show

instance Foldable Maybe' where
  -- fold :: Monoid a => Maybe' a -> a
  fold Nothing' = mempty
  fold (Just' x) = x

  -- foldMap :: Monoid b => (a -> b) -> t a -> b
  foldMap _ Nothing' = mempty
  foldMap f (Just' x) = f x

  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr _ v Nothing' = v
  foldr f v (Just' x) = f x v

  -- foldl :: (a -> b -> a) -> a -> t b -> a
  foldl _ v Nothing' = v
  foldl f v (Just' x) = f v x

instance Functor Maybe' where
  fmap g Nothing' = Nothing'
  fmap g (Just' x) = Just' (g x)

instance Applicative Maybe' where
  pure = Just'
  Nothing'  <*> _  = Nothing'
  (Just' g) <*> mx = fmap g mx

instance Traversable Maybe' where
  -- traverse :: Applicative f => (a -> f b) -> Maybe' a -> f (Maybe' b)
  traverse _ Nothing' = pure Nothing'
  traverse f (Just' x) = pure Just' <*> f x

x = Just' 100 ::  Maybe' (Sum Int)
y = Nothing'  ::  Maybe' (Sum Int)
z = Just' 0   ::  Maybe' (Sum Int)

dec :: Sum Int -> Maybe' Int
dec (Sum n) = if n > 0 then Just' (n-1) else Nothing'

main = do
    putStrLn $ "x = " ++ show(x)
    putStrLn $ "y = " ++ show(y)
    putStrLn $ "z = " ++ show(z)

    putStrLn $ "fold x = " ++ show(fold x)
    putStrLn $ "fold y = " ++ show(fold y)
    putStrLn $ "foldMap (+1) x = " ++ show(foldMap (+1) x)
    putStrLn $ "foldMap (+1) y = " ++ show(foldMap (+1) y)

    putStrLn $ "foldr (+) 1 x = " ++ show(foldr (+) 1 x)
    putStrLn $ "foldr (+) 1 y = " ++ show(foldr (+) 1 y)
    putStrLn $ "foldl (+) 1 x = " ++ show(foldl (+) 1 x)
    putStrLn $ "foldl (+) 1 y = " ++ show(foldl (+) 1 y)

    putStrLn $ "traverse dec x = " ++ show(traverse dec x)
    putStrLn $ "traverse dec y = " ++ show(traverse dec y)
    putStrLn $ "traverse dec z = " ++ show(traverse dec z)

