newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
-- pure:: a -> Z [a]
  pure x = Z [x]
-- <*> :: Z [a->b] -> Z[a] -> Z[b]
  (Z gs) <*> (Z xs) = Z [g x | (g, x) <- gxs]
                        where
                        gxs = zip gs xs

zList1 :: ZipList Int
zList1 = Z [100, 200, 300]
zList2 :: ZipList Int
zList2 = pure 10
zFuncs = Z [(+1), ((-)2), (*3)]

main = do
    putStrLn $ " zList1 = Z [100,200,300]"
    putStrLn $ " zList2 = pure 10"
    putStrLn $ " zFuncs = Z [(+3), (-3), (*3)]"
    putStrLn $ " fmap (+3) zList1 = " ++ show(fmap (+3) zList1)
    putStrLn $ " zList2 = " ++ show(zList2)
    putStrLn $ " zFuncs <*> zList1 = " ++ show(zFuncs <*> zList1)
    putStrLn $ " zFuncs <*> zList2 = " ++ show(zFuncs <*> zList2)
