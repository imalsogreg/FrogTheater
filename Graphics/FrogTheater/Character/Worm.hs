module Worm where
       
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import Data.Fixed
import Control.Concurrent.STM
import Character

instance Character Worm where
  characterStep   dt w = stepWorm dt w
  characterRender w = renderWorm w

type WormSpeedDegPerSec = Float

data WormGoal = Still 
              | MoveCW  WormSpeedDegPerSec
              | MoveCCW WormSpeedDegPerSec
                
data Pos = Pos { latitude :: Float, longitude :: Float }
                
data WormSettings = WormSettings { nSeg             :: Int
                                 , perSegPhaseDelay :: Float
                                 , bodyLength       :: Float
                                 , startPhase       :: Float
                                 , endPhase         :: Float
                                 , liftOffsetPhase  :: Float
                                 , stepFreq         :: Float
                                 }
                                   
                
data WormBody = WormBody { bodySegsPos :: [Pos]
                         , bodyPhase   :: Float }

data Worm = Worm { wormSettings :: WormSettings
                 , stateOfMind :: TVar WormGoal
                 , stateOfBody :: TVar WormBody
                 }

phaseInRange p (r0, r1) = ((p - r0) `mod'` (2*pi)) 
                          < ((r1 - r0) `mod'` (2*pi))

phaseToSubphase :: WormSettings -> Int -> Float -> Float
phaseToSubphase ws i p
  | segP `phaseInRange` (p0,p1)  = pi * ((segP - p0) `mod'` (2*pi)) /
                                   ((p1 - p0) `mod'` (2*pi))
  | otherwise  = 0
  where
    segP = p - perSegPhaseDelay ws * fromIntegral i
    (p0,p1) = (startPhase ws, endPhase ws)

stepWorm :: Float -> Worm -> IO Worm
stepWorm dt w = atomically $ do
  som <- readTVar $ stateOfMind w
  sob <- readTVar $ stateOfBody w
  let thisPhase = bodyPhase sob
      segInds = [0 .. nSeg (wormSettings w) - 1]
      stepSeg :: Pos -> Int -> Pos
      stepSeg (Pos x y) i = Pos (x + xProgress) (y + yProgress)
        where
          xProgress = dt * (sin (phaseToSubphase 
                        (wormSettings w) i thisPhase)^2 * (-100))  --FIX
          yProgress = dt * sin(2 * (phaseToSubphase (wormSettings w) i (thisPhase+pi/7))) * 20
      segPos' = zipWith stepSeg (bodySegsPos sob) segInds
      bodyPhase' = mod' (bodyPhase sob + dt * (stepFreq (wormSettings w))) (2*pi) 
  writeTVar (stateOfBody w) $ WormBody  segPos' bodyPhase'
  return w

initializeWorm :: WormSettings -> (WormGoal, WormBody)
initializeWorm ws = (wormGoal, wormBod)
  where
    x0 = 0
    (n,bLen) = (fromIntegral (nSeg ws), bodyLength ws)
    wormGoal = MoveCCW 10
    wormBod = WormBody { bodySegsPos = [Pos (x*bLen+x0) 0 | x <- [0..n-1]]
                       , bodyPhase = 0 
                       }

    
mkDefaultWorm :: IO Worm
mkDefaultWorm = atomically $ do
  let ws = WormSettings { nSeg = 30
                        , perSegPhaseDelay = 0.05
                        , bodyLength = 5
                        , startPhase = 0
                        , endPhase = pi
                        , liftOffsetPhase = 0
                        , stepFreq = pi
                        }
      (som,sob) = initializeWorm ws
  som' <- newTVar som              
  sob' <- newTVar $ sob
  return $ Worm { wormSettings = ws
                , stateOfMind = som'
                , stateOfBody = sob'
                }
                                                             
    
renderWorm :: Worm -> IO Picture
renderWorm w = do
  s <- atomically $ readTVar (stateOfBody w)
  putStrLn $ "subphase: " ++ show (phaseToSubphase (wormSettings w) 0 (bodyPhase s))
  putStrLn $ "phase: "  ++ show (bodyPhase s)
  atomically $ do
    sob <- readTVar (stateOfBody w)
    return $ Pictures (map renderSeg (bodySegsPos sob))
      where
        renderSeg (Pos x y) = translate x y (Color red (ThickCircle 10 5))
