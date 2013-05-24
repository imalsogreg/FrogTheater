module Worm where
       
import Graphics.Gloss
import Control.Concurrent.STM
import Character

type WormSpeedDegPerSec = Double
data WormGoal = Still 
              | MoveCW  WormSpeedDegPerSec
              | MoveCCW WormSpeedDegPerSec
                
data WormSettings = WormSettings { nSeg :: Int
                                 , startPhase :: Double
                                 , endPhase   :: Double
                                 , liftOffsetPhase :: Double
                                 , squashAmplitudeFraction :: Double
                                 }
                                   
                
data WormBody = WormBody { latitude :: Double
                         , longitude:: Double  
                         , bodyPhase :: Double }

data Worm = Worm { wormSettings :: WormSettings
                 , stateOfMind :: TVar WormGoal
                 , stateOfBody :: TVar WormBody
                 }


instance Character Worm where
  characterRender w t = 

mkDefaultWorm :: IO Worm
mkDefaultWorm = atomically $ do
  som <- newTVar Still              
  sob <- newTVar $ WormBody 0 0 0
  return $ Worm { wormSettings = WormSettings 10 0 0 0 1
                , stateOfMind = som
                , stateOfBody = sob
                }
                                                             