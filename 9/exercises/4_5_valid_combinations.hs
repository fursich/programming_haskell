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

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

--
--

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

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls, rs) <- split ns,
                 l        <- exprs  ls,
                 r        <- exprs  rs,
                 e        <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

totalExpressions :: [Int] -> Int
totalExpressions ns =
  length [e | ns' <- choices ns, e <- exprs ns']

validExpressions ns =
  length [e | ns' <- choices ns, e <- exprs ns', eval e /= []]


valid' :: Op -> Int -> Int -> Bool
valid' Add _ _ = True
valid' Sub x y = True
valid' Mul _ _ = True
valid' Div x y = y /= 0 && x `mod` y == 0

eval' :: Expr -> [Int]
eval' (Val n)     = [n | n > 0]
eval' (App o l r) = [apply o x y | x <- eval' l,
                                   y <- eval' r,
                                   valid' o x y]
validExpressions' ns =
  length [e | ns' <- choices ns, e <- exprs ns', eval' e /= []]

main = do

-- (results)
-- TEST: totalExpressions [2,1] = "10"
-- TEST: totalExpressions [1,2,3,4] = "8500"
-- TEST: totalExpressions [1,3,7,10,25,50] = "33665406"
-- TEST: validExpressions [2,1] = "8"
-- TEST: validExpressions [1,2,3,4] = "3069"
-- TEST: validExpressions [1,3,7,10,25,50] = "4672540"
-- TEST: validExpressions' [2,1] = "9"
-- TEST: validExpressions' [1,2,3,4] = "5143"
-- TEST: validExpressions' [1,3,7,10,25,50] = "10839369"

  TestHelper.testFor "totalExpressions [2,1]" "4_5_valid_combinations"
  TestHelper.testFor "totalExpressions [1,2,3,4]" "4_5_valid_combinations"
  TestHelper.testFor "totalExpressions [1,3,7,10,25,50]" "4_5_valid_combinations"

  TestHelper.testFor "validExpressions [2,1]" "4_5_valid_combinations"
  TestHelper.testFor "validExpressions [1,2,3,4]" "4_5_valid_combinations"
  TestHelper.testFor "validExpressions [1,3,7,10,25,50]" "4_5_valid_combinations"

  TestHelper.testFor "validExpressions' [2,1]" "4_5_valid_combinations"
  TestHelper.testFor "validExpressions' [1,2,3,4]" "4_5_valid_combinations"
  TestHelper.testFor "validExpressions' [1,3,7,10,25,50]" "4_5_valid_combinations"
