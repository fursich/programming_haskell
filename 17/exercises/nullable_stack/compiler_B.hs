import Prelude hiding ((+), (||))
import qualified Prelude as Prelude

-- Operators (for simplicity)
(+) :: Maybe Int -> Maybe Int -> Maybe Int
Just x + Just y = Just (x Prelude.+ y)
Nothing + _     = Nothing
_ + Nothing     = Nothing

(||) :: Maybe Int -> Maybe Int -> Maybe Int
Just x || _       = Just x
Nothing || Just y = Just y

-- Source Language
data Expr = Val Int | Add Expr Expr | Throw | Catch Expr Expr deriving Show

eval :: Expr -> Maybe Int
eval (Val n)     = Just n
eval Throw       = Nothing
eval (Add x y)   = eval x + eval y
eval (Catch x h) = eval x || eval h


type Stack = [Maybe Int]

-- Target Language
data Code = HALT | PUSH (Maybe Int) Code | ADD Code | CATCH Code Code deriving Show

-- Compiler
comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH (Just n) c
comp' Throw c   = PUSH Nothing c
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' (Catch x h) c = comp' x (CATCH (comp h) c)

-- Virtual Machine
exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH x c) s = exec c (x : s)
exec (CATCH c' c) (m : s) = case m of
                                Just n  -> exec c (m : s)
                                Nothing -> exec c (exec c' s)
exec (ADD c) (m : n : s) = exec c (n + m : s)

-- For testing

exp1 :: Expr
exp1 = Catch (Add (Val 5) Throw) (Add (Val 1) (Val 2))

exp2 :: Expr
exp2 = Catch (Add (Val 5) (Val 1)) (Add (Val 1) (Val 2))

exp3 :: Expr
exp3 = Add (Val 1) (Catch (Add (Val 5) Throw) (Val 10))

exp4 :: Expr
exp4 = Add Throw (Catch (Add (Val 5) Throw) (Val 10))

main = do
    putStrLn $ "exp1      = " ++ show(exp1)
    putStrLn $ "comp exp1 = " ++ show(comp exp1)
    putStrLn $ "exec (comp exp1) [] = " ++ show(exec (comp exp1) [])

    putStrLn ""
    putStrLn $ "exp2      = " ++ show(exp2)
    putStrLn $ "comp exp2 = " ++ show(comp exp2)
    putStrLn $ "exec (comp exp2) [] = " ++ show(exec (comp exp2) [])

    putStrLn ""
    putStrLn $ "exp3      = " ++ show(exp3)
    putStrLn $ "comp exp3 = " ++ show(comp exp3)
    putStrLn $ "exec (comp exp3) [] = " ++ show(exec (comp exp3) [])

    putStrLn ""
    putStrLn $ "exp4      = " ++ show(exp4)
    putStrLn $ "comp exp4 = " ++ show(comp exp4)
    putStrLn $ "exec (comp exp4) [] = " ++ show(exec (comp exp4) [])


