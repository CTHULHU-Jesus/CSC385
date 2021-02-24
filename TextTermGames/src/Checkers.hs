module Checkers where

import qualified Data.Vector as Vec
import qualified Data.Matrix as M
import qualified Graphics.Vty as V
import qualified Brick.Main
import qualified Brick.Types
import System.IO.Unsafe (unsafePerformIO)-- be careful!                                         
import System.Random (getStdRandom,randomR)
import Data.Maybe (fromMaybe,catMaybes,fromJust)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Data.List (isInfixOf,intersperse,sortOn)
import Brick.Widgets.Border (borderWithLabel,border,hBorder,vBorder)
import Brick
import qualified Brick.Widgets.List as BWList

-- For Debugging only
import Debug.Trace (trace)

-- Types

data Player
        = White
        | Black
        deriving (Eq,Show)

-- (isKing,Player)
data Checker = NonKing Player
            | King    Player
            deriving (Eq)

type Board = M.Matrix (Maybe Checker)

data State 
        = AISelectorMenu (BWList.List String (Maybe Player))
        | Winner (Maybe Player)
        | State 
            { board :: Board
            , currPlayer :: Player
            , jumpingPiece :: Maybe (Int,Int)
            , currCursorPos :: (Int,Int)
            , aiOponnent :: Maybe Player
            }
-- Constants

initState = State 
    { board = M.matrix 8 8 (\_ -> Nothing)
    , currPlayer = White
    , jumpingPiece = Nothing
    , currCursorPos = (0,0)
    , aiOponnent = Nothing }
    
-- Functions

upBoard  :: Board -> State -> State
upBoard board (State 
            { board = _
            , currPlayer = currPlayer
            , jumpingPiece = jumpingPiece
            , currCursorPos = currCursorPos
            , aiOponnent = aiOponnent
            }) =
    State { board = board
          , currPlayer = currPlayer
          , jumpingPiece = jumpingPiece
          , currCursorPos = currCursorPos
          , aiOponnent = aiOponnent }
upBoard _ x = x

upPlayer :: Player -> State -> State
upPlayer currPlayer (State 
            { board = board
            , currPlayer = _
            , jumpingPiece = jumpingPiece
            , currCursorPos = currCursorPos
            , aiOponnent = aiOponnent
            }) =
    State { board = board
          , currPlayer = currPlayer
          , jumpingPiece = jumpingPiece
          , currCursorPos = currCursorPos
          , aiOponnent = aiOponnent }
upPlayer _ x = x

upJump :: Maybe (Int,Int) -> State -> State
upJump jumpingPiece (State 
            { board = board
            , currPlayer = currPlayer
            , jumpingPiece = _
            , currCursorPos = currCursorPos
            , aiOponnent = aiOponnent
            }) =
    State { board = board
          , currPlayer = currPlayer
          , jumpingPiece = jumpingPiece
          , currCursorPos = currCursorPos
          , aiOponnent = aiOponnent }
upJump _ x = x


upCursor :: (Int,Int) -> State -> State
upCursor currCursorPos (State 
            { board = board
            , currPlayer = currPlayer
            , jumpingPiece = jumpingPiece
            , currCursorPos = _
            , aiOponnent = aiOponnent
            }) =
    State { board = board
          , currPlayer = currPlayer
          , jumpingPiece = jumpingPiece
          , currCursorPos = currCursorPos
          , aiOponnent = aiOponnent }
upCursor _ x = x


upAI     :: Maybe Player -> State -> State
upAI aiOponnent (State 
            { board = board
            , currPlayer = currPlayer
            , jumpingPiece = jumpingPiece
            , currCursorPos = currCursorPos
            , aiOponnent = _
            }) =
    State { board = board
          , currPlayer = currPlayer
          , jumpingPiece = jumpingPiece
          , currCursorPos = currCursorPos
          , aiOponnent = aiOponnent }
upAI _ x = x


draw :: State -> [Widget String] 
draw state@(State 
            { board = board
            , currPlayer = currPlayer
            , jumpingPiece = jumpingPiece
            , currCursorPos = currCursorPos
            , aiOponnent = aiOponnent
            }) = 
    let
        rend :: Board -> String
        rend board = ( init
            . foldl (\a b -> a++"\n"++b) ""
            . map (foldl ++ "")
            . M.toLists
            . M.mapPos (\(y,x) piece ->
            case piece of
                Just x  -> show x
                Nothing -> if (y+x) `mod` 2 then
                               " "
                           else
                               "░"
            $ board))

        label :: String
        label = "Go " ++ show currPlayer
    in
        [ center
        . showCursor "use" (Location currCursorPos)
        . withBorderStyle unicode hBorder
        . str
        . borderWithLabel (str label)
        . rend
        $ board]

-- Instances
instance Show Checker where
    show (NonKing White) = "⬤"
    show (NonKing Black) = "◯"
    show (King White)    = "♚"
    show (King Black)    = "♔"
