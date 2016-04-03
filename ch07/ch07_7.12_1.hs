tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = fst $ divMod x 10
          d     = snd $ divMod xLast 10
