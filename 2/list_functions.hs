screener :: Int -> Bool
screener a = a <= 2 || (odd a)

triple :: Int -> Int
triple a = a * 3

main = do
  print([1,2,3,4,5]);

  print(head([1,2,3,4,5]))
  print(tail([1,2,3,4,5]))
  print([1,2,3,4,5] !! 2)
  print(take 3 [1,2,3,4,5])
  print(drop 3 [1,2,3,4,5])
  print(length [1,2,3,4,5])
  print(sum [1,2,3,4,5])
  print(product [1,2,3,4,5])
  print([1,2,3] ++ [4,5])
  print(reverse [1,2,3,4,5])
  -- appendix B-8 (p284-285)
  print(filter screener [1,2,3,4,5,6,7,8,9])
  print(takeWhile screener [1,2,3,4,5,6,7,8,9])
  print(dropWhile screener [1,2,3,4,5,6,7,8,9])
  print(init [1,2,3,4,5])
  print(splitAt 3 [1,2,3,4,5])
  -- referr to the post below for usage of $
  --  https://typeclasses.com/featured/dollar#:~:text=The%20dollar%20sign%2C%20%24%20%2C%20is,type%20but%20via%20its%20precedence.
  print(take 5 $ repeat 123)
  print(replicate 5 123)
  print(take 5 $ iterate triple 1)
  print(zip [1,2,3] [-1,-2,-3])
  print(map triple [1,2,3,4,5])
  --  print([1,2,3,4,5])
