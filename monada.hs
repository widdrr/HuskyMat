newtype WriterLS a = Writer {runWriter :: (a, [String])}

instance Monad WriterLS where
  return va = Writer (va, [])
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in Writer (vb, log1 ++ log2)


instance  Applicative WriterLS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterLS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterLS () 
tell log = Writer ((), [log])

logIncrementN :: Int->Int -> WriterLS Int
logIncrementN n x = do
    if n == 0 then
        return (x)
    else do
        tell("increment: " ++ show x)
        y <- return (x + 1)
        logIncrementN (n-1) y
