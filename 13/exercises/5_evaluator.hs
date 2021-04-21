import Parser
import Control.Applicative
import Data.Char

data Expr = Val Int | Add Expr Expr | Mul Expr Expr deriving Show

expr :: Parser Expr
expr = do t <- term
          do symbol "+"
             e <- expr
             return (Add t e)
            <|> return t

term :: Parser Expr
term = do f <- factor
          do symbol "*"
             t <- term
             return (Mul f t)
            <|> return f

factor :: Parser Expr
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
           <|> do n <- natural
                  return (Val n)

eval :: String -> Expr
eval xs = case (parse expr xs) of
            [(n, [])]  -> n
            [(_, out)] -> error ("Unused input " ++ out)
            []         -> error "Invalid input"
