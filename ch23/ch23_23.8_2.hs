import Control.Monad.Trans.State hiding (put)

put :: s -> State s ()
put = \s -> state $ \x -> ((), s)

--Prelude> runState (put "blah") "woot"
--((),"blah")