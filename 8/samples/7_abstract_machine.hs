import qualified TestHelper

data Expr = Val Int | Add Expr Expr deriving Show

value' :: Expr -> Int
value' (Val n) = n
value' (Add x y) = value' x + value' y

type Cont = [Op]
data Op = EVAL Expr | ADD Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n: c)   m = exec c (n+m)

value :: Expr -> Int
value e = eval e []

exp1 = Val 2
exp2 = Add (Val 2) (Val 3)
exp3 = Add (Val 5) (Add (Add (Val 2) (Val 3)) (Val 4))

main = do
  putStrLn $ "exp1 = " ++ show(exp1)
  putStrLn $ "exp2 = " ++ show(exp2)
  putStrLn $ "exp3 = " ++ show(exp3)

  TestHelper.testFor "value'(exp1)" "7_abstract_machine"
  TestHelper.testFor "value'(exp2)" "7_abstract_machine"
  TestHelper.testFor "value'(exp3)" "7_abstract_machine"

  TestHelper.testFor "value(exp1)" "7_abstract_machine"
  TestHelper.testFor "value(exp2)" "7_abstract_machine"
  TestHelper.testFor "value(exp3)" "7_abstract_machine"

