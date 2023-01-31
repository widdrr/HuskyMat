import Data.Monoid

-------------------------------------------------------
-- setup pentru tree (din curs)

data BinaryTree a = Leaf a | Node ( BinaryTree a ) ( BinaryTree a )
    deriving Show

foldTree :: ( a -> b -> b ) -> b -> BinaryTree a -> b
foldTree f i ( Leaf x ) = f x i
foldTree f i ( Node l r ) = foldTree f ( foldTree f i r ) l

instance Foldable BinaryTree where
    foldr = foldTree

-------------------------------------------------------
-- exemple pentru testare

none = Nothing
one = Just 3
tree = Node(Node( Leaf 1) ( Leaf 2) ) (Node ( Leaf 3) ( Leaf 4) )
testTree = Leaf Nothing

-- descrierea tuturor functiilor de mai jos se poate gasi pe https://hoogle.haskell.org/
-------------------------------------------------------

elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 a = getAny . foldMap (\x -> Any(x == a))

-------------------------------------------------------

null1 :: (Foldable t) => t a -> Bool
null1 = getAll . foldMap (\x -> All False)

-------------------------------------------------------

length1 :: (Foldable t) => t a -> Int
length1 = getSum . foldMap (\x -> Sum 1) 

-------------------------------------------------------

toList1 :: (Foldable t) => t a -> [a]
toList1 = foldMap (:[])

-------------------------------------------------------
-- Hint: folosiți foldMap

fold1 :: (Foldable t, Monoid m) => t m -> m
fold1 = foldMap id

-------------------------------------------------------
exConst = foldMap Sum (Constant 3)
exConst2 = foldMap Any (Constant False)

data Constant a b = Constant b

instance Foldable (Constant a) where
    foldMap mon (Constant x) = mon x


-------------------------------------------------------
exTwo = foldMap Sum (Two 1 2)

data Two a b = Two a b

instance Foldable (Two a) where
    foldMap mon (Two x y) = mon y

-------------------------------------------------------
exThree = foldMap Sum (Three 1 2 3)

data Three a b c = Three a b c
instance Foldable (Three a b) where
    foldMap mon (Three x y z) = mon z

-------------------------------------------------------
exThree' = foldMap Sum (Three' 1 2 3)

data Three' a b = Three' a b b
instance Foldable (Three' a) where
    foldMap mon (Three' x y z) = mon y <> mon z

-------------------------------------------------------
exFour' = foldMap Sum (Four' 4 5 6 7)

data Four' a b = Four' a b b b
instance Foldable (Four' a) where
    foldMap mon (Four' x y z w) = mon y <> mon z <> mon w

-------------------------------------------------------
exGoat = foldMap Sum (MoreGoats (OneGoat 4) NoGoat (MoreGoats (OneGoat 3) (OneGoat 6) NoGoat))
exGoat2 = foldr (*) 1 (MoreGoats (OneGoat 4) NoGoat (MoreGoats (OneGoat 3) (OneGoat 6) NoGoat))

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Foldable GoatLord where
    foldMap :: Monoid m => (a -> m) -> GoatLord a -> m
    foldMap mon NoGoat = mempty
    foldMap mon (OneGoat a) = mon a
    foldMap mon (MoreGoats x y z) = foldMap mon x <> foldMap mon y <> foldMap mon z