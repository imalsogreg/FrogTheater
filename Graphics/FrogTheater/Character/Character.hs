module Character where

import Graphics.Gloss

class Character c where
  characterStep   :: Float -> c -> IO c
  characterRender :: c -> IO Picture 