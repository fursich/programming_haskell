import qualified TestHelper

data Expr = Val Int | Add Expr Expr deriving Show

folde ::  (Int ->  a) -> (a -> a -> a) -> Expr -> a

folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

eval :: Expr -> Int
eval = folde id  (+)

size :: Expr -> Int
size = folde (const 1) (+)

exp1 :: Expr
exp2 :: Expr
exp3 :: Expr

exp1 = Val 10
exp2 = Add (Val 5) exp1
exp3 = Add (Add exp1 (Val 12)) (Add (Val 15) exp1)

main = do
  putStrLn $ "exp1 = " ++ show(exp1)
  putStrLn $ "exp2 = " ++ show(exp2)
  putStrLn $ "exp3 = " ++ show(exp3)

  TestHelper.testFor "folde (+1) (*) exp1" "5_6_folde"
  TestHelper.testFor "folde (+2) (*) exp2" "5_6_folde"
  TestHelper.testFor "folde (*2) (-) exp3" "5_6_folde"

  TestHelper.testFor "eval exp1" "5_6_folde"
  TestHelper.testFor "eval exp2" "5_6_folde"
  TestHelper.testFor "eval exp3" "5_6_folde"

  TestHelper.testFor "size exp1" "5_6_folde"
  TestHelper.testFor "size exp2" "5_6_folde"
  TestHelper.testFor "size exp3" "5_6_folde"

