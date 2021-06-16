# 1. identifying redex

## 1 + (2 * 3)
2 * 3 -- outmost & innermost redex

## (1 + 2) * (2 + 3)
1 + 2 -- outmost & innermost redex
2 + 3 -- (n/a)

## fst(1 + 2, 2 + 3)
fst(1 + 2, 2 + 3) -- outmost redex
1 + 2 -- innermost redex
2 + 3 -- (n/a)

## (\x -> 1 + x) (2 * 3)
(\x -> 1 + x) (2 * 3) -- outmost redex
2 * 3 -- innermost redex

## 2. fst (1 + 2, 2 + 3)
Innermost evaluation requires to evaluate both `1+2` and `2+3`, the latter of which is not used for later evaluation.
Outmost evaluation would simply ignores 2+3, hence requires one less steps.

## 3. mult = \x -> (\y  ->  x * y)

```
   mult 3 4
= (\x -> (\y  ->  x * y)) 3 4
= (\y -> 3 * y) 4
= 3 * 4
= 12
```

