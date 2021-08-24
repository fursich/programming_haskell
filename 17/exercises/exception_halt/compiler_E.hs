import Prelude hiding (fail)
import qualified Prelude as Prelude

-- Source Language
data Expr = Val Int | Add Expr Expr | Throw | Catch Expr Expr deriving Show

eval :: Expr -> Maybe Int
eval (Val n)     = Just n
eval (Add x y)   = case eval x of
                       Just n -> case eval y of
                           Just m -> Just (n + m)
                           Nothing -> Nothing
                       Nothing -> Nothing
eval Throw       = Nothing
eval (Catch x h) = case eval x of
                       Just n -> Just n
                       Nothing -> eval h

type Stack = [Elem]
data Elem = VAL Int | HAN deriving Show

-- Target Language
data Code = HALT | PUSH Int Code | ADD Code | FAIL Code | MARK Code | UNMARK Code deriving Show

-- Compiler
comp :: Expr -> Code
comp e = comp' e HALT HALT

comp' :: Expr -> Code -> Code -> Code
comp' (Val n) c c'     = PUSH n c
comp' Throw c c'       = FAIL c'
comp' (Catch x h) c c' = MARK(comp' x (UNMARK c) (comp' h c c'))
comp' (Add x y) c c'   = comp' x (comp' y (ADD c) c') c'

-- Virtual Machine
exec :: Code -> Stack -> Stack
exec HALT s                       = s
exec (PUSH x c) s                 = exec c (VAL x : s)
exec (ADD c) (VAL m : VAL n : s)  = exec c (VAL (n + m) : s)
exec (FAIL c) s                   = fail c s
exec (UNMARK c) (VAL n : HAN : s) = exec c (VAL n : s)
exec (MARK c) s                   = exec c (HAN : s)

fail :: Code -> Stack -> Stack
fail c [] = []
fail c (VAL x:s) = fail c s
fail c (HAN : s) = exec c s

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

