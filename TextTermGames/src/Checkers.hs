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
             | King     Player
             deriving (Eq)

type Board = M.Matrix (Maybe Checker)


data State 
        = AISelectorMenu (BWList.List String (Maybe Player))
        | Winner (Maybe Player)
        | State 
            { board :: Board
            , currPlayer :: Player
            , jumpingPiece :: Maybe (Int,Int)
            , move :: Maybe (Int,Int)
            , currCursorPos :: (Int,Int)
            , aiOponnent :: Maybe Player
            }
-- Constants
initState :: State
initState = 
    let
        selectList = Vec.fromList [Nothing, Just White, Just Black]
    in
        AISelectorMenu $ BWList.list "Checkers-AI-List" selectList 2 



initBoard :: State
initBoard = State 
    { board = M.matrix 8 8 
        (\(y,x) -> if (x+y) `mod` 2 == 1 then
                       case y of
                            y' | y' <= 3 -> Just (NonKing Black)
                            y' | y' >= 6 -> Just (NonKing White)
                            _ -> Nothing
                   else
                       Nothing
        )
    , currPlayer = White
    , jumpingPiece = Nothing
    , move = Nothing
    , currCursorPos = (0,0)
    , aiOponnent = Nothing }
    
-- Functions

other :: Player -> Player
other Black = White
other White = Black

upOne :: (Int,Int) -> (Int,Int)
upOne (y,x) = (y+1,x+1)

colorOf :: Checker -> Player
colorOf (NonKing x) = x
colorOf (King    x) = x


upCursor :: (Int,Int) -> State -> State
upCursor currCursorPos (State 
            { board = board
            , currPlayer = currPlayer
            , jumpingPiece = jumpingPiece
            , move = move
            , currCursorPos = _
            , aiOponnent = aiOponnent
            }) =
    State { board = board
          , currPlayer = currPlayer
          , jumpingPiece = jumpingPiece
          , move = move
          , currCursorPos = currCursorPos
          , aiOponnent = aiOponnent }
upCursor _ x = x


upAI     :: Maybe Player -> State -> State
upAI aiOponnent (State 
            { board = board
            , currPlayer = currPlayer
            , jumpingPiece = jumpingPiece
            , move = move
            , currCursorPos = currCursorPos
            , aiOponnent = _
            }) =
    State { board = board
          , currPlayer = currPlayer
          , jumpingPiece = jumpingPiece
          , move = move
          , currCursorPos = currCursorPos
          , aiOponnent = aiOponnent }
upAI _ x = x


draw :: State -> [Widget String] 
draw state@State 
            { board = board
            , currPlayer = currPlayer
            , jumpingPiece = jumpingPiece
            , move = move
            , currCursorPos = currCursorPos
            , aiOponnent = aiOponnent
            } = 
    let
        rend :: Board -> String
        rend m = ( init
            . foldl (\a b -> a++"\n"++b) ""
            . map (foldl (++) "")
            . M.toLists
            . M.mapPos (\(y,x) piece ->
                case piece of
                    Just x  -> show x
                    Nothing -> if 1==(y+x) `mod` 2 then
                                   " "
                               else
                                   "░")
            $ m)

        label :: String
        label = "Go " ++ show currPlayer
    in
        [ center
        . showCursor "use" 
            ( Location 
            . (\(y,x) -> (x+1,y+2))
            $ currCursorPos)
        -- . withBorderStyle unicode hBorder
        . borderWithLabel (str label)
        . str
        . rend
        $ board]

handelEvents :: State -> BrickEvent e () -> EventM e (Next State)
handelEvents state@(State 
            { board = board
            , currPlayer = currPlayer
            , jumpingPiece = jumpingPiece
            , move = move
            , currCursorPos = (cursorY,cursorX)
            , aiOponnent = aiOponnent
            }) e =
    let
        ups    = [V.KChar 'k', V.KUp]
        downs  = [V.KChar 'j', V.KDown]
        rights = [V.KChar 'l', V.KRight]
        lefts  = [V.KChar 'h', V.KLeft]
    in
    case e of
        VtyEvent (V.EvKey key _)-> case key of
            x | x `elem` ups -> 
                Brick.Main.continue
                $ upCursor ((cursorY-1) `mod` 8,cursorX) state

            x | x `elem` downs -> 
                Brick.Main.continue
                $ upCursor ((cursorY+1) `mod` 8,cursorX) state

            x | x `elem` rights -> 
                Brick.Main.continue
                $ upCursor (cursorY,(cursorX+1) `mod` 8) state

            x | x `elem` lefts -> 
                Brick.Main.continue
                $ upCursor (cursorY,(cursorX-1) `mod` 8) state

            V.KEnter -> 
                Brick.Main.continue
                . checkWinner
                . runAI
                . checkWinner
                . update
                $ state

            _ -> Brick.Main.continue state

        _ -> Brick.Main.continue state

handelEvents x e = Brick.Main.continue x

-- Someone can win the game by capturing all of there opponents pieces
-- or by making so there opponent can't move.
checkWinner :: State -> State
checkWinner state@State 
                { board = board
                , currPlayer = _
                , jumpingPiece = _
                , move = _
                , currCursorPos = _
                , aiOponnent = _
                } = 
    let
        piecesActive :: [ Checker ]
        piecesActive = catMaybes
                     . M.toList
                     $ board

        blackActive :: Bool
        blackActive = any (==Black)
                    . map (\x -> case x of
                             NonKing c -> c
                             King    c -> c)
                    $ piecesActive

        whiteActive :: Bool
        whiteActive = any (==White)
                    . map (\x -> case x of
                             NonKing c -> c
                             King    c -> c)
                    $ piecesActive
    in
        if not blackActive then
            Winner $ Just White
        else if not whiteActive then
            Winner $ Just Black
        else if [] == validMoves board Black then
            Winner $ Just White
        else if [] == validMoves board White then
            Winner $ Just Black
        else
            state

checkWinner x = x

update :: State -> State
update state@State 
            { board = board
            , currPlayer = currPlayer
            , jumpingPiece = jumpingPiece
            , move = move
            , currCursorPos = currCursorPos@(cursorY,cursorX)
            , aiOponnent = aiOponnent
            } =
    case move of
        Nothing      -> 
            -- if color of piece == currPlayer then can move
            case board M.! (upOne currCursorPos) of
                Nothing -> state
                Just x  -> if colorOf x == currPlayer then
                               state {move = Just currCursorPos}
                           else
                               state
        Just (y1,x1) -> 
            let
                move = ((y1,x1),currCursorPos)
            in
                if move `elem` (validMoves board currPlayer) then
                    let
                        board' = applyMove move board
                    in
                        if canJump board' currCursorPos then
                            state { board = board'
                                        , move = Just currCursorPos } 
                        else
                            state { board = board'
                                        , move = Nothing
                                        , currPlayer = other currPlayer}
                else
                    state 
update x = x

canJump :: Board -> (Int,Int) -> Bool
canJump board pos =
    case board M.! (upOne pos) of
        Nothing -> False
        Just (NonKing _) -> False
        Just (King    _) -> False-- @TODO

validMoves :: Board -> Player -> [((Int,Int),(Int,Int))]
validMoves board player =
    foldl (++) []
    . M.toList
    . M.mapPos 
        (\(y,x) piece -> 
            case piece of
                Nothing -> []
                Just piece ->
                    if colorOf piece == player then
                        [] -- @TODO makable moves
                    else
                        [])
    $ board


applyMove :: ((Int,Int),(Int,Int)) -> Board -> Board
applyMove move board = -- @TODO
    board
    

runAI :: State -> State
runAI = id

-- Instances
instance Show Checker where
    show (NonKing White) = "⬤"
    show (NonKing Black) = "◯"
    show (King White)    = "♚"
    show (King Black)    = "♔"
