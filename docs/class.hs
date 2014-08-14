
{-# LANGUAGE TypeFamilies, DeriveFunctor #-}
module TestClass where
    
import Control.Monad.Free

class World w where
    type Delta w :: * -> * 
    applyDelta :: Free (Delta w) a -> w -> w
        
data W = W Int
data D n = A n | B n deriving Functor

instance World W where
    type Delta W = D
    applyDelta (Free (A n)) w = 
      let W x2 = applyDelta n w in W $ x2 + 1
    applyDelta (Free (B n)) w =
      let W x2 = applyDelta n w in W $ x2 + 2
    applyDelta (Pure _) w = w
