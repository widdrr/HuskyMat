data List a = Nil
            | Cons a (List a)
        deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List where
    pure x = Cons x Nil
    (<*>) Nil a = Nil
    (<*>) (Cons f fs) Nil = Nil
    (<*>) (Cons f fs) a = fmap f a `concat` (fs <*> a)
        where
            concat Nil b = b
            concat (Cons a xa) b = Cons a (xa `concat` b) 

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

data Cow = Cow {
        name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)


noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty x = Just x


noNegative :: Int -> Maybe Int
noNegative x | x < 0 = Nothing
             | otherwise = Just x

test21 = noEmpty "abc" == Just "abc"
test22 = noNegative (-5) == Nothing 
test23 = noNegative 5 == Just 5

cowFromString :: String -> Int -> Int -> Maybe Cow
-- cowFromString name var greu = if noEmpty name == Nothing || 
--                                     noNegative var == Nothing || 
--                                     noNegative greu == Nothing then Nothing
--                                  else Just (Cow name var greu) 

test24 = cowFromString "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})

cowFromString name var greu = Cow <$> noEmpty name <*> noNegative var <*> noNegative greu

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength x sir = if length sir < x then Just sir else Nothing 

test31 = validateLength 5 "abc" == Just "abc"
mkName :: String -> Maybe Name
mkName sir = Name <$> validateLength 25 sir

mkAddress :: String -> Maybe Address
mkAddress sir = Address <$> validateLength 100 sir

test32 = mkName "Gigel" ==  Just (Name "Gigel")
test33 = mkAddress "Str Academiei" ==  Just (Address "Str Academiei")

mkPerson :: String -> String -> Maybe Person
mkPerson nume adresa = Person <$> mkName nume <*> mkAddress adresa

test34 = mkPerson "Gigel" "Str Academiei" == Just (Person (Name "Gigel") (Address "Str Academiei"))