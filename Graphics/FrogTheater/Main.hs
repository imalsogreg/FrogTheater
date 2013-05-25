module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import Control.Concurrent.STM.TVar
import Character
import Worm

data World = World { worms :: [Worm] }

stepWorld :: ViewPort -> Float -> World -> IO World
stepWorld v dt w = mapM (characterStep dt) (worms w) >>= \w' ->
  return World { worms = w' }

renderWorld :: World -> IO Picture
renderWorld w = do
  ps <- mapM characterRender (worms w)
  return $ Pictures ps
  
main :: IO ()
main = do 
  worm <- myWorm
  simulateIO
    (InWindow "Worm" (600,600) (20,20)) 
    white     
    40
    (World {worms = [worm]})
    renderWorld
    stepWorld

myWorm :: IO Worm
myWorm = mkDefaultWorm