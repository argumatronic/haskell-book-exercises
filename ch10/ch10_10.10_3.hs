myAny f = foldr (\ x y -> f x || y) False
