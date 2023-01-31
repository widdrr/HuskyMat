import Data.List (nub)
import Data.Maybe (fromJust)


type Nume = String
data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  | Prop :->: Prop
  | Prop :<->: Prop
  deriving (Eq, Read)
infixr 2 :|:
infixr 3 :&:

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :|: (Not (Var "P") :|: Not (Var "R")))

instance Show Prop where
    show (Var p) = p
    show F = "False"
    show T = "True"
    show (Not p) = "(~" ++ show p ++ ")"
    show (p :|: q) = "(" ++ show p ++ "|" ++ show q ++ ")"
    show (p :&: q) = "(" ++ show p ++ "&" ++ show q ++ ")"
    show (p :->: q) = "(" ++ show p ++ "->" ++ show q ++ ")"
    show (p :<->: q) = "(" ++ show p ++ "<->" ++ show q ++ ")"

test_ShowProp =
    show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval T _ = True
eval F _ = False
eval (Var name) env = impureLookup name env
eval (Not p) env = not $ eval p env
eval (p :|: q) env = (eval p env) || (eval q env)
eval (p :&: q) env = (eval p env) && (eval q env)
eval (p :->: q) env = (eval (Not p) env) || (eval q env)
eval (p :<->: q) env = (eval (p :->: q) env) && (eval (q :->: p) env)

test_eval = eval (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

variabile :: Prop -> [Nume]

variabile p = nub $ variabile_rec p where
                variabile_rec :: Prop -> [Nume]
                variabile_rec T = []
                variabile_rec F = []
                variabile_rec (Var name) = [name]
                variabile_rec (Not p) = variabile_rec p
                variabile_rec (p :|: q) = variabile_rec p ++ variabile_rec q
                variabile_rec (p :&: q) = variabile_rec p ++ variabile_rec q
                variabile_rec (p :->: q) = variabile_rec p ++ variabile_rec q
                variabile_rec (p :<->: q) = variabile_rec p ++ variabile_rec q

test_variabile =
    variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

envs :: [Nume] -> [Env]
envs [x] = [[(x,False)],[(x,True)]]
envs (x:xs) = [(x,False) : ls| ls <- envs xs] ++ [(x,True) : ls | ls <- envs xs]

test_envs = 
    envs ["P", "Q"]
    ==
    [ [ ("P",False)
      , ("Q",False)
      ]
    , [ ("P",False)
      , ("Q",True)
      ]
    , [ ("P",True)
      , ("Q",False)
      ]
    , [ ("P",True)
      , ("Q",True)
      ]
    ]

satisfiabila :: Prop -> Bool

satisfiabila p = foldr (||) False [eval p e | e <- envs $ variabile p]

test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

valida :: Prop -> Bool

valida p = not $ satisfiabila (Not p)

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True

echivalenta :: Prop -> Prop -> Bool

echivalenta p q = foldr (&&) True [eval (p :<->: q) e | e <- envs $ variabile (p :<->: q)]
test_echivalenta1 =
  True
  ==
  (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 =
  False
  ==
  (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 =
  True
  ==
  (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))