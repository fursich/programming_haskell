data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
-- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

a :: Tree Int
a = Node Leaf 1 (Node Leaf 3 Leaf)
b = fmap (+1) a

main = do
    putStrLn $ "a = " ++ show(a)
    putStrLn $ "fmap (+1) a = " ++ show(fmap (+1) a)
    putStrLn $ "fmap (*3) a = " ++ show(fmap (*3) a)
