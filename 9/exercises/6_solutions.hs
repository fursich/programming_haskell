import qualified TestHelper

data Op = Add | Sub | Mul | Div
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

data Expr = Val Int | App Op Expr Expr
instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                       brak (Val n) = show n
                       brak e       = "("  ++ show e ++ ")"

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

split :: [a] -> [([a], [a])]
split []  =  []
split [_] = []
split (x:xs)= ([x],xs) : [(x:ls,rs) | (ls,rs)  <-  split xs]

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0

type Result = (Expr, Int)

results' :: [Int] -> [Result]
results' [] = []
results' [n] = [(Val n,n) | n > 0]
results' ns =  [res | (ls, rs)  <- split ns,
                      lx       <- results' ls,
                      ry       <- results' rs,
                      res      <- combine'' lx ry]

combine'' :: Result -> Result -> [Result]
combine'' (l, x)  (r, y) =
  [(App o l r, apply o x y) | o <- ops, valid' o x y]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n =
  [e | ns'' <- choices ns, (e, v) <- results' ns'', v == n]

main = do

--  TestHelper.testFor "valid Add 2 3" "6_solutions"
--  TestHelper.testFor "valid Sub 2 3" "6_solutions"
--  TestHelper.testFor "valid Mul 2 3" "6_solutions"
--  TestHelper.testFor "valid Div 2 3" "6_solutions"
--
--  TestHelper.testFor "apply Add 2 3" "6_solutions"
--  TestHelper.testFor "apply Sub 2 3" "6_solutions"
--  TestHelper.testFor "apply Mul 2 3" "6_solutions"
--  TestHelper.testFor "apply Div 2 3" "6_solutions"
--
--  putStrLn $ "show (App Add (Val 1) (App Mul (Val 2) (Val 3))) = " ++ show (App Add (Val 1) (App Mul (Val 2) (Val 3)))
--
--  TestHelper.testFor "eval (App Add (Val 2) (Val 3))" "6_solutions"
--  TestHelper.testFor "eval (App Sub (Val 2) (Val 3))" "6_solutions"
--  TestHelper.testFor "eval (App Mul (Val 2) (Val 3))" "6_solutions"
--  TestHelper.testFor "eval (App Div (Val 2) (Val 3))" "6_solutions"
--
--  TestHelper.testFor "subs [1,2,3]" "6_solutions"
--  TestHelper.testFor "subs [4,3,2,1]" "6_solutions"
--  TestHelper.testFor "interleave 1 [2,3,4]" "6_solutions"
--  TestHelper.testFor "perms [1,2,3]" "6_solutions"
--  TestHelper.testFor "perms [1,2,3,4]" "6_solutions"
--  TestHelper.testFor "choices [1,2,3]" "6_solutions"
--
--  putStrLn $ "exp1 = " ++ show(exp1)
--  TestHelper.testFor "solution exp1 [1,3,5,10] 10" "6_solutions"
--  TestHelper.testFor "solution exp1 [1,3,5,10] 20" "6_solutions"
--  TestHelper.testFor "solution exp1 [1,5,10] 20" "6_solutions"
--
--  TestHelper.testFor "split [1,2,3,4,5]" "6_solutions"
--  TestHelper.testFor "exprs [1,2,3]" "6_solutions"
--  TestHelper.testFor "solutions [1,2,3,4] 1" "6_solutions"
--
--  TestHelper.testFor "results [1,2,3]" "6_solutions"
--  TestHelper.testFor "solutions' [1,2,3,4] 1" "6_solutions"

  TestHelper.testFor "results' [1,2,3]" "6_solutions"
  TestHelper.testFor "solutions'' [1,2,3,4] 1" "6_solutions"

  TestHelper.testFor "solutions'' [1,3,7,10,25,50] 765" "6_solutions"

