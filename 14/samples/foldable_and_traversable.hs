import Data.Foldable

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Foldable Tree where
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

instance Functor Tree where
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

instance Traversable Tree where
  traverse g (Leaf x) = pure Leaf <*> g x
  traverse g (Node l r) = pure Node <*> traverse g l <*> traverse g r

tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
tree2 = Node (Node (Leaf (Just 1)) (Leaf (Just 2))) (Leaf (Just 3))
tree3 = Node (Node (Leaf (Just 1)) (Leaf Nothing)) (Leaf (Just 3))

dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n-1) else Nothing

dec2 :: Int -> Maybe Int
dec2 n = if n > 1 then Just (n-2) else Nothing

main = do
    putStrLn $ "tree = " ++ show(tree)
    putStrLn $ "elem 1 tree = " ++ show(elem 1 tree)
    putStrLn $ "elem 4 tree = " ++ show(elem 4 tree)
    putStrLn $ "toList tree = " ++ show(toList tree)
    putStrLn $ "traverse dec tree = " ++ show(traverse dec tree)
    putStrLn $ "traverse dec2 tree = " ++ show(traverse dec2 tree)

    putStrLn $ "tree2 = " ++ show(tree2)
    putStrLn $ "sequenceA tree2 = " ++ show(sequenceA tree2)
    putStrLn $ "tree3 = " ++ show(tree3)
    putStrLn $ "sequenceA tree3 = " ++ show(sequenceA tree3)


