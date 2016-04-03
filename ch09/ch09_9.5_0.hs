myEnumFromTo :: Enum a => a -> a -> [a]
myEnumFromTo x y
      | xi > yi = []
      | otherwise = x : myEnumFromToi (succ xi) yi
      where xi = fromEnum x
            yi = fromEnum y
            myEnumFromToi a b
                | a > b = []
                | otherwise = (toEnum a) : myEnumFromToi (succ a) b
 
