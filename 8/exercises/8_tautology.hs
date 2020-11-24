import qualified TestHelper

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Eq Prop Prop
          deriving Show

p1 :: Prop
p1 = Or (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Eq (Var 'A') (Not (Var 'A'))

p3 :: Prop
p3 = Eq (And (Not (Var 'A')) (Not (Var 'B'))) (Not (Or (Var 'A') (Var 'B')))

p4 :: Prop
p4 = Eq (Eq (Var 'A') (Var 'B')) (And (Imply (Var 'B') (Var 'A')) (Imply (Var 'A') (Var 'B')))

type Assoc k v = [(k,v)] 
type Subst = Assoc Char Bool

-- 連想配列が完備されていてfindが失敗しない前提とする
find :: Eq a => a -> Assoc a b -> b
find x ((k,v):as) | x == k    = v
                  | otherwise = find x as

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And  p q)  = eval s p && eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Eq p q)    = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Eq p q)    = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

rmdups :: Eq a => [a] -> [a]
rmdups  [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

main = do
  putStrLn $ "p1 = " ++ show(p1)
  putStrLn $ "p2 = " ++ show(p2)
  putStrLn $ "p3 = " ++ show(p3)
  putStrLn $ "p4 = " ++ show(p4)

  TestHelper.testFor "vars(p1)" "8_tautology"
  TestHelper.testFor "vars(p2)" "8_tautology"
  TestHelper.testFor "vars(p3)" "8_tautology"
  TestHelper.testFor "vars(p4)" "8_tautology"

  TestHelper.testFor "bools 1" "8_tautology"
  TestHelper.testFor "bools 3" "8_tautology"

  TestHelper.testFor "isTaut p1" "8_tautology"
  TestHelper.testFor "isTaut p2" "8_tautology"
  TestHelper.testFor "isTaut p3" "8_tautology"
  TestHelper.testFor "isTaut p4" "8_tautology"

