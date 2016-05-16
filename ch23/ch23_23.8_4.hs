eval :: State s a -> s -> a
eval (State sa) = ???

--Prelude> eval get "bunnicula"
--"bunnicula"
--Prelude> eval get "stake a bunny"
--"stake a bunny"