data Woot

data Blah

f :: Woot -> Blah
f = undefined 

g :: (Blah, Woot) -> (Blah, Blah)
g (x, y) = (x, f y)
