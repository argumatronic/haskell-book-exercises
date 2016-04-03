data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)


convert :: Quantum -> Bool

convert1 Yes = True
convert1 No = True
convert1 Both = True

convert2 Yes = True
convert2 No = True
convert2 Both = False

convert3 Yes = True
convert3 No = False
convert3 Both = False

convert4 Yes = False
convert4 No = False
convert4 Both = False

convert5 Yes = False
convert5 No = False
convert5 Both = True

convert6 Yes = False
convert6 No = True
convert6 Both = True

convert7 Yes = False
convert7 No = True
convert7 Both = False

convert8 Yes = True
convert8 No = False
convert8 Both = True