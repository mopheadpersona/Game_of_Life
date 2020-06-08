module Main where

import Brick
import Data.List
import qualified Brick.Types as T
import Brick.Util (on)
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Center (center)
import Control.Monad (void)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes.Color as Color

type Cell = (Int, Int)

data ScreenState = ScreenState
  { curPos :: Cell
  , curHighlighted :: Bool
  , genCell :: [Cell]
  }

initState :: ScreenState
initState = ScreenState {curPos = (0, 0), curHighlighted = True, genCell = []}

lifeStep :: [Cell] -> [Cell]
lifeStep cells = [head g | g <- grouped cells, viable g]
  where grouped = group . sort . concatMap neighbors
        neighbors (x, y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0)]
        viable [_,_,_] = True
        viable [c,_] = c `elem` cells
        viable _ = False

renderApp :: ScreenState -> [Widget n]
renderApp s = [center $ B.borderWithLabel title (mainLayer s)]

boardSize :: Cell
boardSize = (height, width)

height :: Int
height = 25

width :: Int
width = 60

tup :: a -> b -> (a, b)
tup a b = (a, b)

layer :: Int -> ScreenState -> Widget n
layer i s = foldr1 (<+>) $ map (letter s) $ tup <$> [i] <*> [0..(width - 1)]

layers :: [a]
layers = []

makeLayers s = [layers ++ [layer i s] | i <- [0..(height - 1)]]

mainLayer s = foldr1 (<=>) $ concat $ makeLayers s

title :: Widget n
title = str "Game of Life"

cursorHl = attrName "cursorHl"

letter :: ScreenState -> Cell -> Widget n
letter s i
  | curPos s == i = str "X"
  | i `elem` genCell s = str "X"
  | otherwise = str " "


handleEvent s (T.VtyEvent (V.EvKey k [])) =
  case k of
    V.KEsc   -> halt s 
    V.KLeft  -> continue $ s {curPos = if (snd (curPos s) > 0)          then (fst (curPos s), snd (curPos s) - 1) else (curPos s)}
    V.KRight -> continue $ s {curPos = if (snd (curPos s) < width - 1)  then (fst (curPos s), snd (curPos s) + 1) else (curPos s)}
    V.KUp    -> continue $ s {curPos = if (fst (curPos s) > 0)          then (fst (curPos s) - 1, snd (curPos s)) else (curPos s)}
    V.KDown  -> continue $ s {curPos = if (fst (curPos s) < height - 1) then (fst (curPos s) + 1, snd (curPos s)) else (curPos s)}
    V.KChar 'x' -> continue $ s {genCell = genCell s ++ [curPos s]}
    V.KChar 'c' -> continue $ s {genCell = []}
    V.KChar ' ' -> continue $ s {genCell = lifeStep (genCell s)}
    _ -> continue s 
handleEvent s _ = continue s 

app :: App ScreenState e ()
app =
  App
    { appDraw = renderApp
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const $ attrMap V.defAttr [(cursorHl, Color.black `on` Color.cyan)]
    }

main :: IO ()
main = void $ defaultMain app initState