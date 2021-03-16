{-
 -- commented out as it dupicates with original (->) definition in Prelude

instance Monad ((->) a) where
-- return:: b -> (a -> b)
  return = const
-- >>= :: (a -> b) -> (b -> a -> c) -> (a -> c)
  f >>= g = /x -> g (f x) x
-}

type UnitCost = Int
type TaxRate  = Float

isAffordable :: TaxRate -> UnitCost -> Bool
isAffordable x y = (1.0 + x) * realToFrac(y) <= 1000.0

calcTaxRate :: UnitCost -> TaxRate
calcTaxRate x | x <= 100   = 0.02
              | x <= 500   = 0.15
              | x <= 800   = 0.20
              | otherwise = 0.40

main = do
    putStrLn $ "calcTaxRate 700 = " ++ show(calcTaxRate  700)
    putStrLn $ "calcTaxRate 800 = " ++ show(calcTaxRate  800)
    putStrLn $ "calcTaxRate 900 = " ++ show(calcTaxRate  900)

    putStrLn $ "(calcTaxRate >>= isAffordable) 700 = " ++ show((calcTaxRate >>= isAffordable) 700)
    putStrLn $ "(calcTaxRate >>= isAffordable) 800 = " ++ show((calcTaxRate >>= isAffordable) 800)
    putStrLn $ "(calcTaxRate >>= isAffordable) 900 = " ++ show((calcTaxRate >>= isAffordable) 900)
