import Control.Monad.Trans.State hiding (get)

get :: State s s
get = state $ \s -> (s, s)

--Prelude> runState get "curryIsAmaze"
--("curryIsAmaze","curryIsAmaze")