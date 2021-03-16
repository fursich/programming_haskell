data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
-- fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var x) = Var (f x)
  fmap f (Val x) = Val x
  fmap f (Add ex ey) = Add (fmap f ex) (fmap f ey)

instance Applicative Expr where
-- pure :: a -> Expr a
  pure x = Var x
-- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  _ <*> Val x         = Val x
  Val x <*> _         = Val x
  Var f <*> Var x     = Var (f x)
  Var f <*> Add ex ey = Add (fmap f ex) (fmap f ey)
  Add fx gx <*> ex    = Add (fx <*> ex) (gx <*> ex)

instance Monad Expr where
-- return :: a -> Expr a
  return = pure
-- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  Var x >>= g     = g x
  Val x >>= _     = Val x
  Add ex ey >>= g = Add (ex >>= g) (ey >>= g)

exp1 :: Expr Char
exp1 = Add (Add (Var 'a') (Val 2)) (Add (Add (Var 'b') (Val 10)) (Var 'c'))

f x | x == 'a' = 1
    | x == 'b' = 2
    | x == 'c' = 3

exp2 :: Expr Char
exp2 = pure 'x'

exp3 :: Expr (Char -> Int)
exp3 = Add (Val 1) (Var f)

g :: Char -> Expr Char
g x | x == 'a' = Val 3
    | x == 'b' = Val 5
    | x == 'c' = Val 10

main = do
    putStrLn $ "exp1 = " ++ show(exp1)
    putStrLn $ "fmap f exp1 = " ++ show(fmap f exp1)

    putStrLn $ "exp2 (= pure 'x') = " ++ show(exp2)
    putStrLn $ "Add((Val 1) (Var f)) <*> exp1 = " ++ show(exp3 <*> exp1)

    -- (>>=) represents assignment/replacement of the original variables with other expressions
    putStrLn $ "exp1 >>= g = " ++ show(exp1 >>= g)

