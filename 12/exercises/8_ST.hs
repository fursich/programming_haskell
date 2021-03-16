type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap f stx = do
    x <- stx
    return (f x)

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do
    f <- stf
    x <- stx
    return (f x)

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S(\s ->
     let (x, s') = app st s in app (f x) s')


stBool :: ST Bool
stBool = S (\x -> ( x > 0, -x))

stringifyBool :: Bool -> String
stringifyBool x | x         = "True"
                | otherwise = "False"

stStr = fmap stringifyBool stBool

stFunc :: ST (Bool -> String)
stFunc = S(\x -> (stringifyBool, x * 2))

stStringifyBool :: Bool -> ST String
stStringifyBool x | x         = S (\y -> ("True",  y))
                  | otherwise = S (\y -> ("False", -y))

main = do
    putStrLn $ "app stBool 1 = "    ++ show(app stBool 1)
    putStrLn $ "app stBool (-1) = " ++ show(app stBool (-1))

    putStrLn $ "app stStr 1 = "    ++ show(app stStr 1)
    putStrLn $ "app stStr (-1) = " ++ show(app stStr (-1))

    putStrLn $ "app (stFunc <*> stBool) 2 = "     ++ show(app (stFunc <*> stBool) 2)
    putStrLn $ "app (stFunc <*> stBool) (-2) = "  ++ show(app (stFunc <*> stBool) (-2))

    putStrLn $ "app (stBool >>= stStringifyBool) 2 = "     ++ show(app (stBool >>= stStringifyBool) 2)
    putStrLn $ "app (stBool >>= stStringifyBool) (-2) = "     ++ show(app (stBool >>= stStringifyBool) (-2))

