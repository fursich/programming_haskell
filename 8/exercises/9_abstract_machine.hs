import qualified TestHelper

data Expr = Val Int | Add Expr Expr | Mul Expr Expr deriving Show

value' :: Expr -> Int
value' (Val n) = n
value' (Add x y) = value' x + value' y
value' (Mul x y) = value' x * value' y

type Cont = [Op]
data Op = EVAL Expr | ADD | MUL | VAL Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : ADD : c)
eval (Mul x y) c = eval x (EVAL y : MUL : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (VAL n : c)
exec (VAL n : ADD : c) m = exec c (n+m)
exec (VAL n : MUL : c) m = exec c (n*m)

value :: Expr -> Int
value e = eval e []

exp1 = Val 2
exp2 = Add (Val 2) (Val 3)
exp3 = Add (Val 5) (Add (Add (Val 2) (Val 3)) (Val 4))
exp4 = Mul (Val 3) (Val 4)
exp5 = Mul (Val 5) (Add (Mul (Val 2) (Val 3)) (Val 4))

main = do
  putStrLn $ "exp1 = " ++ show(exp1)
  putStrLn $ "exp2 = " ++ show(exp2)
  putStrLn $ "exp3 = " ++ show(exp3)
  putStrLn $ "exp4 = " ++ show(exp4)
  putStrLn $ "exp5 = " ++ show(exp5)

  TestHelper.testFor "value'(exp1)" "9_abstract_machine"
  TestHelper.testFor "value'(exp2)" "9_abstract_machine"
  TestHelper.testFor "value'(exp3)" "9_abstract_machine"
  TestHelper.testFor "value'(exp4)" "9_abstract_machine"
  TestHelper.testFor "value'(exp5)" "9_abstract_machine"

  TestHelper.testFor "value(exp1)" "9_abstract_machine"
  TestHelper.testFor "value(exp2)" "9_abstract_machine"
  TestHelper.testFor "value(exp3)" "9_abstract_machine"
  TestHelper.testFor "value(exp4)" "9_abstract_machine"
  TestHelper.testFor "value(exp5)" "9_abstract_machine"

