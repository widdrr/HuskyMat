newtype ReaderWriter env a = RW {getRW :: env -> (a, String)}

instance Monad (ReaderWriter env) where
  ma >>= k = RW f
    where
      f env =
        let (va, string1) = getRW ma env
            (vb, string2) = getRW (k va) env
         in (vb, string1++", "++ string2)

-- Instanta Applicative, Functor si functia <*> scrisa explicit
-- nu sunt obligatorii la examen, le am scris ca sa testez si
-- sa nu mai am eroare.
instance Applicative (ReaderWriter env) where
  pure :: a -> ReaderWriter env a
  pure = return

  (<*>) :: ReaderWriter env (a -> b) -> ReaderWriter env a -> ReaderWriter env b
  mf <*> ma = do
    f <- mf
    f <$> ma

instance Functor (ReaderWriter env) where
  fmap :: (a -> b) -> ReaderWriter env a -> ReaderWriter env b
  fmap f ma = f <$> ma
