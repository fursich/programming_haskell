{-
 -- commented out as it dupicates with original (->) definition in Prelude

instance Applicative ((->) a) where
-- pure:: b -> (a -> b)
  pure = const
-- <*> :: (a -> b -> c) -> (a -> b) -> (a -> c)
  f <*> g = \x -> f x (g x)
-}

type UnitCost = Int
type TaxRate  = Float

isAffordable :: UnitCost -> TaxRate -> Bool
isAffordable x y = realToFrac(x) * (1.0 + y) <= 1000.0

calcTaxRate :: UnitCost -> TaxRate
calcTaxRate x | x <= 100   = 0.02
              | x <= 500   = 0.15
              | x <= 800   = 0.20
              | otherwise = 0.40

comment :: UnitCost -> Bool -> String
comment u b = show(u) ++ " is " ++ assess
                where
                assess | b         = "reasonable"
                       | otherwise = "too expensive"

main = do
    putStrLn $ "calcTaxRate 700 = " ++ show(calcTaxRate 700)
    putStrLn $ "calcTaxRate 800 = " ++ show(calcTaxRate 800)
    putStrLn $ "calcTaxRate 900 = " ++ show(calcTaxRate 900)

    putStrLn $ "(isAffordable <*> calcTaxRate) 700 = " ++ show((isAffordable <*> calcTaxRate) 700)
    putStrLn $ "(isAffordable <*> calcTaxRate) 800 = " ++ show((isAffordable <*> calcTaxRate) 800)
    putStrLn $ "(isAffordable <*> calcTaxRate) 900 = " ++ show((isAffordable <*> calcTaxRate) 900)

    putStrLn $ "(comment <*> isAffordable <*> calcTaxRate) 700 = " ++ (comment <*> (isAffordable <*> calcTaxRate)) 700
    putStrLn $ "(comment <*> isAffordable <*> calcTaxRate) 800 = " ++ (comment <*> (isAffordable <*> calcTaxRate)) 800
    putStrLn $ "(comment <*> isAffordable <*> calcTaxRate) 900 = " ++ (comment <*> (isAffordable <*> calcTaxRate)) 900

