hunsD :: Integral a => a -> a
hunsD x = d
    where x1 = fst $ divMod x 100
          d  = snd $ divMod x1 10
