import qualified TestHelper

data Maybe' a = Nothing' | Just' a
instance Eq a => Eq (Maybe' a) where
   Nothing' == Nothing' = True
   Nothing' == Just' x = False
   Just' x == Nothing' = False
   Just' x == Just' y = x == y

data ArrayOf a = Array [a]
listOf :: ArrayOf a -> [a]
listOf (Array xs) = xs

instance Eq a => Eq (ArrayOf a) where
  Array [] == Array ys = null(ys)
  Array xs == Array [] = null(xs)
  Array(x:xs) == Array(y:ys) = x == y && (Array xs) == (Array ys)

main = do
  TestHelper.testFor "(Nothing' :: Maybe' Int) == (Nothing' :: Maybe' Int)" "7_instance"
  TestHelper.testFor "Nothing' == Just' 0" "7_instance"
  TestHelper.testFor "Just' 7 == Nothing'" "7_instance"
  TestHelper.testFor "Just' 5 == Just' 4" "7_instance"
  TestHelper.testFor "Just' 3 == Just' 3" "7_instance"

  TestHelper.testFor "(Array [] :: ArrayOf Int) == (Array [] :: ArrayOf Int)" "7_instance"
  TestHelper.testFor "Array [1] == Array []" "7_instance"
  TestHelper.testFor "Array [] == Array [2,3]" "7_instance"
  TestHelper.testFor "Array [1] == Array [2]" "7_instance"
  TestHelper.testFor "Array [3] == Array [3]" "7_instance"
  TestHelper.testFor "Array [1,2,3,4,5] == Array [2,1,3,4,5]" "7_instance"
  TestHelper.testFor "Array [1,2,3,4,5] == Array [1,2,3,4,5]" "7_instance"

