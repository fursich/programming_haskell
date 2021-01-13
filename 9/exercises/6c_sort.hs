import qualified TestHelper
import Data.List

data Op = Add | Sub | Mul | Div deriving Eq
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

instance Ord Op where
  compare x y = compare (elemIndex x ordList) (elemIndex y ordList)
                where ordList = [Mul, Div, Add, Sub] -- prioritize * / over + - to make sorted results apparent

data Expr = Val Int | App Op Expr Expr deriving Eq
instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                       brak (Val n) = show n
                       brak e       = "("  ++ show e ++ ")"

instance Ord Expr where
  compare (Val x) (Val y)               = compare x y
  compare (Val _) (App _ _ _)           = LT
  compare (App _ _ _) (Val _)           = GT
  compare (App op1 x y) (App op2 x' y') | op1 /= op2         = compare op1 op2
                                        | compare x x' /= EQ = compare x x'
                                        | otherwise          = compare y y'


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
valid' Div x y = y > 1 && x `mod` y == 0

type Result = (Expr, Int)

results' :: [Int] -> [Result]
results' [] = []
results' [n] = [(Val n,n) | n > 0]
results' ns =  [res | (ls, rs) <- split ns,
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

sortSolutions :: [Int] -> Int -> [Expr]
sortSolutions ns n = sort (solutions'' ns n)

sortSolutionsBy :: (Expr -> Expr -> Ordering) -> [Int] -> Int -> [Expr]
sortSolutionsBy s ns n = sortBy s (solutions'' ns n)

val1 = Val 1
val2 = Val 2

exp1 = App Mul val1 val2
exp2 = App Add val1 val2

revCmp :: Expr -> Expr -> Ordering
revCmp (App op1 x y) (App op2 x' y') | revCmp y y' /= EQ = revCmp y' y
                                     | revCmp x x' /= EQ = revCmp x' x
                                     | otherwise         = compare op2 op1
revCmp (App _ _ _) (Val _)           = LT
revCmp (Val _) (App _ _ _)           = GT
revCmp (Val x) (Val y)               = compare y x

countVal :: Expr -> Int
countVal (Val x)      = 1
countVal (App op x y) = countVal x + countVal y

cmpByVal :: Expr -> Expr -> Ordering
cmpByVal e1 e2 | countVal e1 /= countVal e2 = compare (countVal e1) (countVal e2)
               | otherwise                  = compare e1 e2


main = do
  TestHelper.testFor "choices [1,2]" "6c_sort"
  TestHelper.testFor "sort [val1, val2, val1, val2, val1]" "6c_sort"
  TestHelper.testFor "sort [exp1, exp2, exp1]" "6c_sort"
  TestHelper.testFor "val1 < val2" "6c_sort"
  TestHelper.testFor "val1 < exp1" "6c_sort"
  TestHelper.testFor "exp1 < exp2" "6c_sort"
  TestHelper.testFor "Mul < Div" "6c_sort"
  TestHelper.testFor "Mul < Add" "6c_sort"
  TestHelper.testFor "Mul < Sub" "6c_sort"
  TestHelper.testFor "Add < Sub" "6c_sort"

  TestHelper.testFor "sortSolutions [1,2,3] 1" "6c_sort"
  TestHelper.testFor "sortSolutionsBy revCmp [1,2,3] 1" "6c_sort"
  TestHelper.testFor "sortSolutionsBy cmpByVal [1,2,3] 1" "6c_sort"

  TestHelper.testFor "sortSolutions [1,2,3,4] 6" "6c_sort"
  TestHelper.testFor "sortSolutionsBy revCmp [1,2,3,4] 6" "6c_sort"
  TestHelper.testFor "sortSolutionsBy cmpByVal [1,2,3,4] 6" "6c_sort"

