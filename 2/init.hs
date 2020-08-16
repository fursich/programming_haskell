init' a = take (length a - 1) a
init'' = reverse . tail . reverse

main = do
  print(init' [1,2,3,4,5]);
  print(init'' [1,2,3,4,5]);

  print(init' [1]);
  print(init'' [1]);
