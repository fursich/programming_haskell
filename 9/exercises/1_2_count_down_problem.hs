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

choices' :: [a] -> [[a]]
choices' xs = [x | xss <- subs xs,
                   x   <- perms xss]

remainings :: Eq a => a -> [a] -> [[a]]
remainings _ []  = []
remainings x (y:ys) | x == y    = [ys]
                    | otherwise = map(y:) (remainings x ys)


isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x:xs) ys = map(isChoice xs) (remainings x ys) == [True]


main = do

  TestHelper.testFor "choices [1,2,3]" "1_2_count_down_problem"
  TestHelper.testFor "choices [1,2,3]" "1_2_count_down_problem"
  TestHelper.testFor "choices' [1,2,3,4]" "1_2_count_down_problem"
  TestHelper.testFor "choices' [1,2,3,4]" "1_2_count_down_problem"

  TestHelper.testFor "remainings 1 [1,2,3,4]" "1_2_count_down_problem"
  TestHelper.testFor "remainings 2 [2,1,2,3,2,4]" "1_2_count_down_problem"
  TestHelper.testFor "remainings 4 [2,1,2,3,2,4]" "1_2_count_down_problem"
  TestHelper.testFor "remainings 4 [2,1]" "1_2_count_down_problem"

  TestHelper.testFor "isChoice [1] [2,1,2,3,2,4]" "1_2_count_down_problem"
  TestHelper.testFor "isChoice [2,4] [2,1,2,3,2,4]" "1_2_count_down_problem"
  TestHelper.testFor "isChoice [2,2,4] [2,1,2,3,2,4]" "1_2_count_down_problem"
  TestHelper.testFor "isChoice [2,4,4] [2,1,2,3,2,4]" "1_2_count_down_problem"
  TestHelper.testFor "isChoice [2,4,3] [2,1,2,3,2,4]" "1_2_count_down_problem"
  TestHelper.testFor "isChoice [2,4,3] [2]" "1_2_count_down_problem"
