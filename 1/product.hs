product' :: Num a => [a] -> a
product' [] = 1
product' (num:nums) = num * product'(nums)

main = print(product'([2..4]))
