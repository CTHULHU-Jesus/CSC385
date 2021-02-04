module Main where

import Brick
import Lib 

main :: IO ()
main = 
	let 
		-- app :: App Games () e
		app = App {
			-- :: Games -> [Widget e]
			appDraw           = menu
			-- :: Games -> [CursorLocation e] -> Maybe (CursorLocation e)
			, appChooseCursor = \_ -> showCursorNamed "use"
			-- :: Games -> BrickEvent e () -> EventM e (Next Games)
			, appHandleEvent  = handelEvents 
			-- :: Games -> EventM e Games
			, appStartEvent   = return
			-- :: Games -> AttrMap 
			, appAttrMap      = noMap
		}
	in
		do
			_ <- defaultMain app initalState
			return ()
