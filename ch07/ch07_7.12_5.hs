g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)

-- *Main> g show (1, 2)
-- ("1",2)