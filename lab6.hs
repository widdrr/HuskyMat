data Fruct
  = Mar String Bool
  | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]

ePortocalaDeSicilia :: Fruct -> Bool

ePortocalaDeSicilia (Portocala nume _) = elem nume ["Tarocco","Moro","Sanguinello"]
ePortocalaDeSicilia _ = False

nrFeliiSicilia :: [Fruct] -> Int

nrFeliiSicilia lst = sum $ [felii | (Portocala nume felii) <- lst, ePortocalaDeSicilia (Portocala nume felii)]

nrMereViermi :: [Fruct] -> Int

nrMereViermi lst = length $ [vierme | (Mar _ vierme)<-lst, vierme]

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

vorbeste :: Animal -> String

--merge case
vorbeste ani = case ani of
                (Pisica _) -> "Meow!"
                (Caine _ _) -> "Woof!"

rasa :: Animal -> Maybe String

rasa ani = case ani of
                (Pisica _) -> Nothing
                (Caine _ rasa) -> Just rasa

data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

verifica :: Matrice -> Int -> Bool

verifica (M linii) n = foldr (&&) True [(foldr (+) 0 ln) == n | L ln <- linii]

doarPozN :: Matrice -> Int -> Bool

doarPozN (M linii) n = foldr (&&) True [(length $ filter (<=0) ln )== 0  | L ln <- linii, length ln == n]

corect :: Matrice -> Bool

corect (M []) = True
corect (M ((L ln):[])) = True 
corect (M ((L ln):linii)) = length ln == lung (M linii)
                            where 
                                lung :: Matrice -> Int
                                lung (M ((L ln):[])) = length ln
                                lung (M ((L ln):linii)) = if length ln == lung (M linii) then length ln else -1

corect2 :: Matrice -> Bool
corect2 (M []) = True
corect2 (M ((L ln1):linii)) = foldr (\x y -> if x == y then x else -1) (length $ ln1) [length ln | L ln <- linii] /= -1