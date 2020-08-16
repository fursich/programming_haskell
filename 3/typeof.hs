import Data.Typeable

hasType x y = show(typeOf(x)) == y

main = do
    print(['a', 'b', 'c'] `hasType` "[Char]")
    print(('a', 'b', 'c') `hasType` "(Char,Char,Char)")
    print([(False, '0'), (True, '2')] `hasType` "[(Bool,Char)]")
    print(([False, True], ['0', '1']) `hasType` "([Bool],[Char])")

funcArr :: [[a] -> [a]]
funcArr = [tail, init, reverse]
{-
:type [tail, init, reverse]
-- [[a] -> [a]]
-}
