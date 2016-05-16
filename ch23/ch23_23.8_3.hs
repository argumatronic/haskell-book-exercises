import Control.Monad
import Control.Monad.Trans.State

exec :: State s a -> s -> s
exec (State sa) s = snd $ sa s

--Prelude> exec (put "wilma") "daphne"
--"wilma"
--Prelude> exec get "scooby papu"
--"scooby papu"