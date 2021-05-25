import Data.Foldable
import Data.Monoid

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Foldable Tree where
  -- fold :: Monoid a => Tree a -> a
  fold Leaf = mempty
  fold (Node l x r) = fold l `mappend` x `mappend` fold r

  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap _ Leaf = mempty
  foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ v Leaf = v
  foldr f v (Node l x r) = foldr f (f x  (foldr f v r)) l

  -- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl _ v Leaf = v
  foldl f v (Node l x r) = foldl f (f (foldl f v l) x) r

instance Functor Tree where
  fmap g Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf = pure Leaf
  traverse f (Node l x r) = pure Node <*> traverse f l <*> f x <*> traverse f r

x = Leaf :: Tree (Sum Int)
y = Node (Leaf) (Sum 5) (Node (Leaf) (Sum 4) (Leaf)) :: Tree (Sum Int)

dec :: Sum Int -> Maybe Int
dec (Sum n) = if n > 0 then Just (n-1) else Nothing

main = do
    putStrLn $ "x = " ++ show(x)
    putStrLn $ "y = " ++ show(y)

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

