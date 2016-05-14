import Control.Applicative

hurr :: Num a => a -> a
hurr = (*2)

durr :: Num a => a -> a
durr = (+10)

m :: Num a => a -> a
m = hurr . durr

m' :: Integer -> Integer
m' = fmap hurr durr

m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

hurrDurr :: Integer -> Integer
hurrDurr = do
    a <- hurr
    b <- durr
    return (a + b)