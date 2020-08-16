second xs = head (tail xs)
second :: [a] -> a

swap (x,y) = (y,x)
swap :: (a, b) -> (b, a)

pair x y = (x, y)
pair :: x -> y ->  (x, y)

double x = x * 2
double :: Num x => x -> x

palindrome xs = reverse xs == xs
palindrome :: Eq x => [x] -> Bool

twice f x = f $ f x
twice :: (x -> x) -> x -> x

main = print "all type defs have been validated by the compiler"
