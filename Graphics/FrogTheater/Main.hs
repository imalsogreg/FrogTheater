module Main where

import Graphics.Gloss
import Control.Concurrent.STM.TVar
import Character
import Worm

type model = World { worms :: [Worm] }

renderWorld :: model -> Picture
renderWorld w = map characterRender (worms w)

main :: IO ()
main = do 
  worm <- myWorm
  animate (InWindow "Worm" (600,600) (20,20))
    white $ characterRender worm
       
myWorm :: IO Worm
myWorm = mkDefaultWorm