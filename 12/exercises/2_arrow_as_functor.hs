{-
 -- commented out as it dupicates with original (->) definition in Prelude

instance Functor ((->) a) where
-- fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap = (.)
-}


lessThanThree :: (->) Int Bool
lessThanThree x | x < 3     = True
                | otherwise = False

lessThanThreeInFraction = fmap lessThanThree floor

main = do
    putStrLn $ "lessThanThree 2 = " ++ show(lessThanThree 2)
    putStrLn $ "lessThanThree 3 = " ++ show(lessThanThree 3)
    putStrLn $ "lessThanThree 5 = " ++ show(lessThanThree 5)

    putStrLn $ "lessThanThreeInFraction 3.2 = " ++ show(lessThanThreeInFraction 3.2)
    putStrLn $ "lessThanThreeInFraction 2.9 = " ++ show(lessThanThreeInFraction 2.9)

