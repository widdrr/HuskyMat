import Data.List
import Data.Char

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (x:xs)
    | x == reverse x = vocale x + nrVocale xs
    | otherwise = nrVocale xs
    where
        vocale :: String -> Int
        vocale [] = 0
        vocale (x:xs)
            | elem x "aeiouAEIOU" = 1 + vocale xs
            | otherwise = vocale xs

addAfterEven :: Int -> [Int] -> [Int]
addAfterEven t [] = []
addAfterEven t (x:xs)
    | x `mod` 2 == 0 = x : t : (addAfterEven t xs)
    | otherwise = x : (addAfterEven t xs)

divisors :: Int -> [Int]
divisors x = [y | y <- [1..x], x `mod` y == 0]

listaDiv :: [Int] -> [[Int]]
listaDiv xs = [divisors x | x <- xs]

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalComp :: Int -> Int -> [Int] -> [Int]

inIntervalRec x y [] = []
inIntervalRec x y (z:zs)
    | x <= z && z <= y = z : inIntervalRec x y zs
    | otherwise = inIntervalRec x y zs

inIntervalComp x y zs = [z | z <- zs, x <= z && z <= y]

pozitiveRec :: [Int] -> Int
pozitiveComp :: [Int] -> Int

pozitiveRec [] = 0
pozitiveRec (x:xs)
    | x > 0 = 1 + pozitiveRec xs
    | otherwise = pozitiveRec xs

pozitiveComp xs = sum [1 | x <- xs, x > 0]
--descrierile de liste nu au niciun fel de a agrega elementele listei
-- poti folosi legth sau sum

pozitiiImpareRecSecret :: Int -> [Int] -> [Int]
pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareComp :: [Int] -> [Int]

--nu e cea mai eleganta solutie
-- e perfecta
pozitiiImpareRecSecret p [] = []
pozitiiImpareRecSecret p (x:xs)
    | x `mod` 2 == 1 = p : pozitiiImpareRecSecret (p+1) xs
    | otherwise = pozitiiImpareRecSecret (p+1) xs

pozitiiImpareRec = pozitiiImpareRecSecret 0

pozitiiImpareComp xs = [p | (x,p) <- zip xs [0..], x `mod` 2 == 1 ]
-- poti pune [0..], zip se opreste cand se termina cel mai scurt sir

multDigitsRec :: String -> Int
multDigitsComp :: String -> Int

multDigitsRec [] = 1
multDigitsRec (x:xs)
    | isNum x = (ord x - ord '0') * multDigitsRec xs
    | otherwise = multDigitsRec xs
    where
        isNum x = ord '0' <= ord x && ord x <= ord '9' --stiu ca exista isDigit
-- atunci de ce nu il folosesti?
-- ma distram sa vad daca merge asa
-- ok! :)

multDigitsComp xs = product [ord x - ord '0' | x <- xs ,isDigit x]

-- te-am notat