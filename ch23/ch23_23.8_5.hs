import Control.Monad.Trans.State hiding (modify)

modify :: (s -> s) -> State s ()
modify f = state $ \s -> ((), f s)

--Prelude> runState (modify (+1)) 0
--((),1)
--Prelude> runState (modify (+1) >> modify (+1)) 0
--((),2)