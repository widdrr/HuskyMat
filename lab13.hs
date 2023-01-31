{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing


instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

fct :: Maybe Int ->  Maybe Bool
fct  mx =  mx  >>= (\x -> Just (pos x))

fct mx = do
    x <- mx
    return (pos(x))

--o sa sar peste a implementa in alte feluri si trec la monade
addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = mx >>= (\x -> (my >>= (\y -> return (x + y))))

addM' mx my = do
    x <- mx
    y <- my
    return (x + y)


cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))

cartesian_product' xs ys = do
    x <- xs
    y <- ys
    return (x,y)

prod f xs ys = [f x y | x <- xs, y <- ys]

prod' f xs ys = do
    x <- xs
    y <- ys
    return (f x y)

myGetLine :: IO String
myGetLine = getChar >>= \x ->
      if x == '\n' then
          return []
      else
          myGetLine >>= \xs -> return (x:xs)

myGetLine' = do
    x <- getChar
    if x == '\n' then
        return []
    else do
        xs <- myGetLine'
        return (x : xs)

prelNo noin =  sqrt noin

ioNumber = do
     noin  <- readLn :: IO Float
     putStrLn $ "Intrare\n" ++ (show noin)
     let  noout = prelNo noin
     putStrLn $ "Iesire"
     print noout

ioNumber' = (readLn :: IO Float) >>= 
    (\noin -> (putStrLn $ "Intrare\n" ++ (show noin))
    >>(let noout = prelNo noin in ((putStrLn $ "Iesire") >> print noout)))

--- Monada Writer

newtype WriterS a = Writer { runWriter :: (a, String) } 


instance  Monad WriterS where
  return va = Writer (va, "")
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterS () 
tell log = Writer ((), log)

logIncrement :: Int -> WriterS Int
logIncrement x = do
    tell("increment: " ++ show x ++ " \n")
    return (x + 1)
    
logIncrement2 :: Int -> WriterS Int
logIncrement2 x = do
    y <- logIncrement x
    logIncrement y

logIncrementN :: Int->Int -> WriterS Int
logIncrementN n x = do
    if n == 0 then
        return (x)
    else do
        y <- logIncrement x
        logIncrementN (n-1) y
------
--exercitiu in celalalt fisier
------   
data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN p = "NAME: " ++ name p 
showPersonA :: Person -> String
showPersonA p = "AGE: " ++ (show $ age p)

{-
showPersonN $ Person "ada" 20
"NAME: ada"
showPersonA $ Person "ada" 20
"AGE: 20"
-}

showPerson :: Person -> String
showPerson p = "(" ++ showPersonN p ++ ", " ++ showPersonA p ++ ")" 

{-
showPerson $ Person "ada" 20
"(NAME: ada, AGE: 20)"
-}


newtype Reader env a = Reader { runReader :: env -> a }


instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env

instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance Functor (Reader env) where              
  fmap f ma = pure f <*> ma   

--am stat mai mult decat as vrea sa recunosc pe astea
--voiam sa fac in stilul "normal" de monada in care
--fac chain de monade pentru a suprapune efectele
--nu a mers deoarece instanta Reader data nu permite accesarea env (i.e ask)
--pentru a putea accesa env(p) din mshowPerson
--fac bind intre un Reader Person Person si o functie Person -> Reader Person String

--nu stiu daca asta era scopul exercitiului

mshowPersonN :: Reader Person String
mshowPersonN = Reader(\p -> p) 
               >>= (\p -> return ("NAME: " ++ name p))
mshowPersonA ::  Reader Person String
mshowPersonA = Reader(\p -> p) 
               >>= (\p -> return ("AGE: " ++ (show $ age p)))

--aici as fi putut face un chain de monade dar era foarte urat
mshowPerson ::  Reader Person String
mshowPerson = Reader(\p -> p) 
              >>= (\p -> return ("(" ++ runReader mshowPersonN p ++ "," ++ runReader mshowPersonA p++ ")"))

--solutie care nici macar nu foloseste binding dar a fost prima idee si e draguta

{-
mshowPersonN :: Reader Person String
mshowPersonN = Reader(\p -> "NAME: " ++ name p) 

mshowPersonA ::  Reader Person String
mshowPersonA = Reader(\p -> "AGE: " ++ (show $ age p)) 

mshowPerson ::  Reader Person String
mshowPerson = Reader(\p -> "(" ++ runReader mshowPersonN p ++ ", " ++ runReader mshowPersonA p ++ ")") 
-}
{-
runReader mshowPersonN  $ Person "ada" 20
"NAME:ada"
runReader mshowPersonA  $ Person "ada" 20
"AGE:20"
runReader mshowPerson  $ Person "ada" 20
"(NAME:ada,AGE:20)"
-}