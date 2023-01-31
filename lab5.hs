sumaPatrImp :: [Int] -> Int
sumaPatrImp = foldr ((+).(^2)) 0 . filter odd

allTrue :: [Bool] -> Bool
allTrue = foldr (&&) True

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies pred = foldr ((&&) . pred) True

anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies pred = foldr ((||) . pred) False

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr mp = foldr ((:) . mp) [] 

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr fltr = foldr (\h t -> if fltr h then h : t else t) []

listToInt :: [Integer] -> Integer 
listToInt = foldl ((+) . (*10)) 0

rmChar :: Char -> String -> String
rmChar c = filter (/=c)

rmCharsRec :: String -> String -> String 
rmCharsRec [] x = x 
rmCharsRec (c:cs) x = rmCharsRec cs $ rmChar c x

rmCharsFold :: String -> String -> String
--rmCharsFold blist = flip (foldr (rmChar)) blist
rmCharsFold c = foldr (\ h t -> if (rmChar h c) == c then h : t else t ) [] --practic folosesc rmChar doar ca sa verific ca h nu e in c

rmCharsFold2 :: String -> String -> String
rmCharsFold2 s1 s2 = foldr (\c str -> if (rmChar c s1 ) /= s1 then str else c : str)  [] s2