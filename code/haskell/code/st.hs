import Control.Monad.ST
import Data.STRef
bar :: Int
bar = runST (do x <- newSTRef 1
                modifySTRef x (+ 2)
                readSTRef x)

