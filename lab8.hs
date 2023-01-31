data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
 toArb :: a -> Arb
 fromArb :: Arb -> a

instance Show Punct where
    show (Pt []) = "()"
    show (Pt (x:xs)) = "(" ++ show x ++ (foldr (\x xs -> (", " ++ show x ++ xs) ) ")" xs)

instance ToFromArb Punct where
    toArb (Pt []) = Vid
    toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))
    fromArb Vid = Pt []
    fromArb (F x) = Pt [x]
    fromArb (N arb1 arb2) = (fromArb arb1) +++ (fromArb arb2)
                            where (+++) (Pt xs1) (Pt xs2) = Pt (xs1 ++ xs2)

data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

instance GeoOps Geo where
  perimeter x = case x of
                    Square l -> 4*l
                    Rectangle w h -> 2*w + 2*h
                    Circle r -> 2*pi*r
  area x = case x of
                    Square l -> l*l
                    Rectangle w h -> w*h
                    Circle r -> pi*r*r

instance (Eq a,Floating a) => Eq (Geo a) where
  (==) g1 g2 = (perimeter g1) == (perimeter g2)