import qualified TestHelper

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

data Weight = Balanced Int | Unbalanced deriving (Eq, Show)

compareWeight :: Tree a -> Weight
compareWeight (Leaf x) = Balanced 1
compareWeight (Node x y) = addWeight (compareWeight x) (compareWeight y)

addWeight :: Weight -> Weight -> Weight
addWeight Unbalanced x              = Unbalanced
addWeight x Unbalanced              = Unbalanced
addWeight (Balanced x) (Balanced y) | x >= y -1 && x <= y + 1 = Balanced (x + y)
                                    | otherwise               = Unbalanced

balanced :: Tree a ->  Bool
balanced x | compareWeight x == Unbalanced = False
           | otherwise = True

halve :: [a] -> ([a], [a])
halve xs = splitAt n xs
            where n = (length xs) `div` 2

balance :: [a] -> Tree a
balance (x:xs) | null xs   = Leaf x
               | otherwise = Node l r
                   where hss = halve (x:xs)
                         l   = balance (fst hss)
                         r   = balance (snd hss)

t1 :: Tree Int
t1 = Node (Node (Node (Leaf 0) (Leaf 1)) (Leaf 4))
         (Node (Leaf 6) (Node (Leaf 7) (Leaf 9)))

t2 :: Tree Int
t2 = Node (Node (Node (Leaf 0) (Leaf 1)) (Leaf 4))
         (Node (Leaf 6) (Node (Node (Leaf 7) (Leaf 8)) (Leaf 9)))

l0 = [0]
l1 = [1,2,3,4,5,6,7,8,9,10]
l2 = [10,12,11,43,55]

main = do
  putStrLn $ "t1 = " ++ show(t1)
  putStrLn $ "t2 = " ++ show(t2)

  TestHelper.testFor "balanced t1" "3_4_balance"
  TestHelper.testFor "balanced t2" "3_4_balance"

  putStrLn $ "l1 = " ++ show(l1)
  putStrLn $ "l2 = " ++ show(l2)

  TestHelper.testFor "balance l0" "3_4_balance"
  TestHelper.testFor "balanced $ balance l0" "3_4_balance"
  TestHelper.testFor "balance l1" "3_4_balance"
  TestHelper.testFor "balanced $ balance l1" "3_4_balance"
  TestHelper.testFor "balance l2" "3_4_balance"
  TestHelper.testFor "balanced $ balance l2" "3_4_balance"
