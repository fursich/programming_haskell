fac' :: Int -> Int
fac' 0 = 1
fac' n | n > 0 = n * fac'(n-1)

-- *Main> fac' (-10)
-- *** Exception: fac.hs:(2,1)-(3,30): Non-exhaustive patterns in function fac'
