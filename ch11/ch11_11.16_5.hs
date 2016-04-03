data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add exp1 exp2) = eval exp1 + eval exp2


printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add exp1 exp2) = printExpr exp1 ++ " + " ++ printExpr exp2
