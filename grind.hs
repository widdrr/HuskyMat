import Data.List
import Data.Binary

newtype Point = Pt [Int]
    deriving (Show, Eq)

data Arb = Empty | Node Int Arb Arb
    deriving (Show, Eq)

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

instance ToFromArb Point where
    toArb x = toArbRec x Empty where
        
        insertArb :: Int -> Arb -> Arb
        insertArb x Empty = Node x Empty Empty
        insertArb x (Node y left right) = if x <= y then Node y (insertArb x left) right
                                          else Node y left (insertArb x right)
        toArbRec :: Point -> Arb -> Arb
        toArbRec (Pt []) arb = arb
        toArbRec (Pt (x:xs)) arb = toArbRec (Pt xs) (insertArb x arb)

    fromArb Empty = Pt []
    fromArb (Node x left right) = fromArb left +++ Pt [x] +++ fromArb right where
        (+++) :: Point -> Point -> Point
        (+++) (Pt x) (Pt y) = Pt (x ++ y)

x = Pt [6,3,2,4,7,1,2]
y = Node 6 (Node 3 (Node 2 (Node 1 Empty (Node 2 Empty Empty)) Empty) (Node 4 Empty Empty)) (Node 7 Empty Empty)

sortPt (Pt x) = Pt (sort x)
test11 = y == toArb x
test12 = fromArb y == sortPt x

getFromInterval :: Int->Int->[Int]->[Int]
getFromInterval x y= filter (\e -> e <= y && e >= x)

getFromInterval2 :: Int->Int->[Int]->[Int]
getFromInterval2 x y l = do
    e <- l
    if e <= y && e >= x then return e 
    else []

getFromInterval3 :: Int->Int->[Int]->[Int]
getFromInterval3 x y l = l >>= (\e -> if e <= y && e >= x then return e else [])

myFunc:: Int -> [String]
myFunc x = [show x | i <- [1..x]]

-- data ReaderWriter env a = RW (env -> (a, String))
-- getRW ::ReaderWriter env a -> (env -> (a, String))
-- getRW (RW f) = f
myFunc3 :: Int -> (Int,String)
myFunc3 x = (x + 42, "Added 42 to " ++ show x)
--data ReaderWriter env a = RW {getRW :: env -> (a,String)}
newtype ReaderWriter env a = RW {getRW :: env -> (a,String)}
ask :: ReaderWriter env env

ask = RW (\env -> (env,""))

instance Monad (ReaderWriter env) where
    return x = RW (\_ -> (x,""))
    (>>=) :: ReaderWriter env a -> (a -> ReaderWriter env b) -> ReaderWriter env b
    matey >>= f = RW newGetRW where
        newGetRW env = let (va,s1) = getRW matey env
                           (vb,s2) = getRW (f va) env
                       in (vb, s1 ++", apoi "++ s2)
    (>>) matey boaba = boaba

instance Applicative (ReaderWriter env) where
    pure = return
    mf <*> ma = do
        f <- mf
        va <- ma
        return (f va) 

instance Functor (ReaderWriter env) where
    fmap f ma = pure f <*> ma

boaba :: Int -> ReaderWriter Int Int
boaba n = RW(\env ->(env + n,"Bagat Boaba " ++ show n))

manhwa :: Int -> ReaderWriter Int Int
manhwa n = RW(\env ->(env - n,"Citit Manhwa " ++ show n))

nebunie :: Int -> Int -> ReaderWriter Int Int
nebunie n1 n2 = do
    x <- ask
    y <- boaba n1
    z <- manhwa n2
    return (x*y*z)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)


newtype State state val = State{runState:: state->(val,state)}

instance Monad (State state) where
    return val = State(\s->(val,s))
    (>>=) :: State state a -> (a -> State state b) -> State state b
    ma >>= f = State (\state-> 
                        let (va,newstate) = runState ma state
                            (vb,updstate) = runState (f va) newstate
                        in (vb,updstate))

instance Applicative (State state) where
    pure = return
    mf <*> ma = do
        f <- mf 
        a <- ma 
        return (f a)

instance Functor (State state) where
    fmap f ma = pure f <*> ma

get :: State state state
get = State (\s -> (s,s))
modify :: (state -> state) -> State state ()
modify f = State (\s -> ((),f s))

cMULTIPLIER, cINCREMENT :: Word32

cMULTIPLIER = 1664525
cINCREMENT = 1013904223

rnd , rnd2 :: State Word32 Word32
rnd = do
    modify (\seed -> cMULTIPLIER *seed + cINCREMENT)
    Main.get
rnd2 = do
    r1 <- rnd
    r2 <- rnd
    return ( r1 + r2 )