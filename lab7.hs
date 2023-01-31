data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
           deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)

instance Show Expr where
    show (Const x) = show x
    show (e1 :*: e2) = show e1 ++ "*" ++ show e2
    show (e1 :+: e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    
exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)


evalExpr :: Expr -> Int

evalExpr (Const x) = x
evalExpr (e1 :*: e2) = evalExpr e1 * evalExpr e2
evalExpr (e1 :+: e2) = evalExpr e1 + evalExpr e2

arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

evalArb :: Tree -> Int

evalArb (Lf l) = l
evalArb (Node Mult t1 t2) = evalArb t1 * evalArb t2
evalArb (Node Add t1 t2) = evalArb t1 + evalArb t2

expToArb :: Expr -> Tree

expToArb (Const x) = Lf x
expToArb (e1 :*: e2) = Node Mult (expToArb e1) (expToArb e2)
expToArb (e1 :+: e2) = Node Add (expToArb e1) (expToArb e2)


class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert
      :: Ord key
      => key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  values :: c key value -> [value]
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value
  
  keys coll = [key | (key,values) <- toList coll]
  values coll = [values | (key,values) <- toList coll]
  fromList = foldr (uncurry insert) empty

newtype PairList k v
  = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
    empty = PairList []

    singleton key value = PairList [(key,value)]

    insert key value (PairList list) = PairList ((key,value): (getPairList $ delete key (PairList list)))

    clookup key (PairList list) = lookup key list

    delete key (PairList list) = PairList (filter (\(k,v) -> k /= key) list)
    
    toList (PairList list) = list

data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

instance Collection SearchTree where

    empty = Empty
    singleton key value = BNode Empty key (Just value) Empty
    
    insert key value Empty = singleton key value 
    insert key value (BNode st1 k mval st2)
        | key == k = BNode st1 k (Just value) st2
        | key < k = BNode (insert key value st1) k mval st2
        | otherwise = BNode st1 k mval (insert key value st2)
    
    clookup key Empty = Nothing
    clookup key (BNode st1 k mval st2)
        | key == k = mval
        | key < k = clookup key st1
        | otherwise = clookup key st2

    delete key Empty = empty
    delete key (BNode st1 k mval st2)
        | key == k = BNode st1 k Nothing st2
        | key < k = BNode (delete key st1) k mval st2
        | otherwise = BNode st1 k mval (delete key st2)
    
    toList Empty = []
    toList (BNode st1 k Nothing st2) = toList st1 ++ toList st2
    toList (BNode st1 k (Just val) st2) = toList st1 ++ [(k,val)]  ++ toList st2 