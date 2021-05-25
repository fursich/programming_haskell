import Data.Foldable
import Data.Monoid

filterF :: Foldable t => (a -> Bool) -> t a -> [a]

filterF f = filter f . toList

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Foldable Tree where
  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap _ Leaf = mempty
  foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r


even :: Sum Int -> Bool
even (Sum x) = Prelude.even x

x = Node Leaf (Sum 5) (Node (Node Leaf (Sum 10) Leaf) (Sum 4) Leaf) :: Tree (Sum Int)

main = do
    putStrLn $ "x = " ++ show(x)
    putStrLn $ "toList x = " ++ show(toList x)
    putStrLn $ "filterF even x = " ++ show(filterF Main.even x)

