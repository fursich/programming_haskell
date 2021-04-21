import Parser
import Control.Applicative
import Data.Char

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
            <|> do
             symbol "-"
             e <- expr
             return (t - e)
            <|> return t

term :: Parser Int
term = do e <- expt
          do symbol "*"
             t <- term
             return (e * t)
            <|> do
             symbol "/"
             t <- term
             return (e `div` t)
            <|> return e

expt = do f <- factor
          do symbol "^"
             e <- expt
             return (f ^ e)
           <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
           <|> integer

eval :: String -> Int
eval xs = case (parse expr xs) of
            [(n, [])]  -> n
            [(_, out)] -> error ("Unused input " ++ out)
            []         -> error "Invalid input"
