import Parser
import Control.Applicative
import Data.Char

-- a
-- expr ::= expr - nat | nat
-- nat := 0 | 1 | 2 | ...

-- b
--   expr :: Parser Int
--   expr = do e <- expr
--             symbol "-"
--             n <- natural
--             return (e - n)
--           <|> natural
-- 
--   eval :: String -> Int
--   eval xs = case (parse expr xs) of
--               [(n, [])]  -> n
--               [(_, out)] -> error ("Unused input " ++ out)
--               []         -> error "Invalid input"

-- c : compilable, but infinite loop occurs when evaluated an expression at runtime

-- d
expr :: Parser Int
expr = do xs <- many (do
                  n <- natural
                  symbol  "-"
                  return n)
          x <- natural

          return $ foldl (-) (head xs) $ tail xs ++ [x]

eval :: String -> Int
eval xs = case (parse expr xs) of
            [(n, [])]  -> n
            [(_, out)] -> error ("Unused input " ++ out)
            []         -> error "Invalid input"

