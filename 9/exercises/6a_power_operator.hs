import qualified TestHelper

data Op = Add | Sub | Mul | Div | Pow
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "^"

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
apply Pow x y = x ^ y

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
valid' Div x y = y > 1 && x `mod` y == 0
valid' Pow x y = x > 1 && y > 1

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
ops = [Add, Sub, Mul, Div, Pow]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n =
  [e | ns'' <- choices ns, (e, v) <- results' ns'', v == n]

main = do
  TestHelper.testFor "choices [1,2]" "6a_power_operator"

  TestHelper.testFor "results' [1,2]" "6a_power_operator"
  TestHelper.testFor "results' [1,2,3]" "6a_power_operator"
  TestHelper.testFor "results' [1,2,3,4]" "6a_power_operator"

  TestHelper.testFor "solutions'' [1,2] 1" "6a_power_operator"
  TestHelper.testFor "solutions'' [1,2,3] 8" "6a_power_operator"
  TestHelper.testFor "solutions'' [1,3,7,10,25,50] 765" "6a_power_operator"

