import qualified TestHelper

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

occurs :: Ord a => a -> Tree a -> Bool

occurs x (Leaf y)     = x == y
occurs x (Node l y r) = case compare x y of
                          EQ -> True
                          LT -> occurs x l
                          GT -> occurs x r
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

main = do
  putStrLn $ "t = " ++ show(t)
  TestHelper.testFor "occurs 1 t" "2_occurs"
  TestHelper.testFor "occurs 2 t" "2_occurs"
  TestHelper.testFor "occurs 6 t" "2_occurs"
  TestHelper.testFor "occurs 8 t" "2_occurs"
  TestHelper.testFor "occurs 9 t" "2_occurs"
