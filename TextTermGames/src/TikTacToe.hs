module TikTacToe where

import Data.Matrix

data Player = O
		    | X
		    deriving (Eq)

other :: Player -> Player
other O -> X
other X -> O

data State = 
	State {
		board 	   :: Matrix (Maybe Color),
		currPlayer :: Player
		currCursorPos :: Int
	}

init :: State
init = State 
		{ board = matrix 3 3 (\_ -> Nothing)
		, currPlayer = O
		, currCursorPos = 1
		}
