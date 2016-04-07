import Data.Monoid (Monoid, (<>))

newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend Mem {runMem = f} Mem {runMem = g} = Mem $ mappend f g


f' = Mem $ \s -> ("hi", s + 1)
main = do
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0