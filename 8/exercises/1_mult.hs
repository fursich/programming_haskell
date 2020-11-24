import qualified TestHelper

data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

mul :: Nat -> Nat -> Nat
mul Zero     n = Zero
mul (Succ m) n = add (mul m n) n


main = do
  TestHelper.testFor "add Zero Zero" "1_mult"
  TestHelper.testFor "add (Succ(Zero)) (Succ(Succ(Zero)))" "1_mult"
  TestHelper.testFor "nat2int( add (int2nat 1) (int2nat  2) )" "1_mult"
  TestHelper.testFor "mul (Succ(Succ(Succ(Zero)))) (Succ(Succ(Zero)))" "1_mult"
  TestHelper.testFor "nat2int ( mul (int2nat 3) (int2nat 2) )" "1_mult"
