
```haskell
split :: [a] -> [([a], [a])]
split []  =  []
split [_] = []
split (x:xs)= ([x],xs) : [(x:ls,rs) | (ls,rs)  <-  split xs]

-- 以下のように変更する
split' :: [a] -> [([a], [a])]
split' [] = []
split' [x]  =  [([], x), (x, [])]
split' (x:xs)= ([],(x:xs)) : [(x:ls,rs) | (ls,rs)  <-  split xs]

-- > split [1,2,3]
-- => [([],[1,2,3]),([1],[2,3]),([1,2],[3]),([1,2,3],[])]

-- このとき以下の動作がどのようになるか
exprs' :: [Int] -> [Expr]
exprs' []  = []
exprs' [n] = [Val n]
exprs' ns  = [e | (ls, rs) <- split' ns,
                  l        <- exprs'  ls, -- lsにns自身を含むケースが生まれる
                  r        <- exprs'  rs, -- rsにns自身を含むケースが生まれる
                  e        <- combine l r]
```

splitの挙動の変更により、split xsの結果は、タプルの一方に元のxs自身を含む要素を持つ。

つまりexprs'の定義において、(ls, rs) <- split' ns のlsまたはrsがns自身を含むことになり、再帰が終わらなくなるため無限ループになる

