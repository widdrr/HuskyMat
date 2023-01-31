factori :: Int -> [Int]

factori n = [x | x <-[1..n], n `rem` x == 0 ]

prim :: Int -> Bool

prim n
    | length (factori n) == 2 = True
    | otherwise = False

numerePrime :: Int -> [Int]

numerePrime n = [x | x <- [2..n], prim x]

myZip3 :: [a]->[b]->[c]->[(a,b,c)]

myZip3 [] y z = []
myZip3 x [] z = []
myZip3 x y [] = []
myZip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : myZip3 xs ys zs

firstEl :: [(a,b)] -> [a]

firstEl = map (\(x,y) -> x)

sumList :: [[Int]] -> [Int]

sumList = map sum

prel2 :: [Int] -> [Int]

prel2 = map doubleIf
    where 
        doubleIf x
            | x `mod` 2 == 0 = x `div` 2
            |otherwise = x * 2

filterByChar :: Char -> [String] -> [String]

filterByChar x = filter (elem x)

impSquares :: [Int] -> [Int]

impSquares = map (\x -> x*x) . filter (\x -> x `mod` 2 == 1)

impPosSquares :: [Int] -> [Int]

impPosSquares = map (\(x,i) -> x*x ) . filter (\(x,i) -> i `mod` 2 == 1) . (`zip` [0..])

numaiVocale :: [String] -> [String]

numaiVocale = map $ filter $ (`elem` "aeiouAEIOU")

myMap :: ( a -> b ) -> [a] -> [b]

myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: ( a -> Bool ) -> [a] -> [a]

myFilter _ [] = []
myFilter f (x:xs)
    | f x = x : myFilter f xs
    | otherwise = myFilter f xs