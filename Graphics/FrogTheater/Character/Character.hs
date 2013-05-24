module Character where

import Graphics.Gloss

class Character c where
  characterRender :: c -> time -> Picture