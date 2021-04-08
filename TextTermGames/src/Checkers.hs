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
            , forcedMove :: Bool
            , currCursorPos :: (Int,Int)
            , aiOponnent :: Maybe Player
            }
-- Constants
initState :: State
initState = 
    let
        selectList = Vec.fromList [Nothing, Just White, Just Black]
    in
      initBoard Nothing
        -- Changed for prototpye
        -- AISelectorMenu $ BWList.list "Checkers-AI-List" selectList 2 



initBoard :: Maybe Player -> State
initBoard ai = State 
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
    , forcedMove = False
    , currCursorPos = (0,0)
    , aiOponnent = ai }
    
-- Functions

other :: Player -> Player
other Black = White
other White = Black

upOne :: (Int,Int) -> (Int,Int)
upOne (y,x) = (y+1,x+1)

colorOf :: Checker -> Player
colorOf (NonKing x) = x
colorOf (King    x) = x

{--
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
upAI aiOponnent State 
            { board = board
            , currPlayer = currPlayer
            , jumpingPiece = jumpingPiece
            , move = move
            , currCursorPos = currCursorPos
            , aiOponnent = _
            } =
    State { board = board
          , currPlayer = currPlayer
          , jumpingPiece = jumpingPiece
          , move = move
          , currCursorPos = currCursorPos
          , aiOponnent = aiOponnent }
upAI _ x = x
--}

draw :: State -> [Widget String] 
draw (AISelectorMenu lst) =
    let
        render :: Bool -> Maybe Player -> Widget String
        render b x = str $ (if b then "->" else "  ")
            ++ case x of
                Nothing ->     " 2Player"
                Just White  -> " AI [White]"
                Just Black  -> " AI [Black]"
    in
        [ center
        . border
        $ BWList.renderList render True lst]
draw (Winner s) = 
    let
        txt = case s of
                Nothing -> "Tie Game"
                Just x  -> "Congrats " ++ show x
    in
        [ center 
        . border 
        $ str txt]

draw state@State 
            { board = board
            , currPlayer = currPlayer
            , jumpingPiece = jumpingPiece
            , move = move
            , currCursorPos = currCursorPos
            , aiOponnent = aiOponnent
            } = 
    let
        rend :: Board -> Widget String
        rend m = ( 
            vBox
            . map hBox
            . M.toLists
            . M.mapPos (\(y,x) piece ->
                case piece :: Maybe Checker of
                    Just piece -> 
                      case move of
                        Just select | select==(y-1,x-1) -> 
                          str . highlightShow $ piece
                        _ -> str . show $ piece 
                    Nothing    -> if 1==(y+x) `mod` 2 then
                                   str " "
                               else
                                   str "░")
            $ m)

        label :: String
        label = "Go " ++ show currPlayer
    in
        [ center
        . showCursor "use" 
            ( Location 
            . (\(y,x) -> (x+1,y+1))
            $ currCursorPos)
        -- . withBorderStyle unicode hBorder
        . (case move of
            Nothing -> id
            Just (y,x) -> id)
                -- @TODO show that selected pice was selected
                -- showCursor "use" (Location (y+1,x+1)))
        . borderWithLabel (str label)
        . rend
        $ board]


handelEvents :: State -> BrickEvent e () -> EventM e (Next State)
handelEvents (AISelectorMenu lst) event = 
    case event of
            -- up 
        (VtyEvent (V.EvKey (V.KChar 'k') _)) -> Brick.Main.continue (AISelectorMenu 
                                                    $ BWList.listMoveUp lst)
        (VtyEvent (V.EvKey V.KUp _))         -> Brick.Main.continue (AISelectorMenu 
                                                    $ BWList.listMoveUp lst)
            -- down
        (VtyEvent (V.EvKey (V.KChar 'j') _)) -> Brick.Main.continue (AISelectorMenu 
                                                    $ BWList.listMoveDown lst)
        (VtyEvent (V.EvKey V.KDown  _))      -> Brick.Main.continue (AISelectorMenu 
                                                    $ BWList.listMoveDown lst)
            -- enter
        (VtyEvent (V.EvKey V.KEnter _))      -> (case BWList.listSelectedElement lst of
            Nothing -> Brick.Main.continue (AISelectorMenu lst)
            Just (_,option) -> Brick.Main.continue $ initBoard option
            )
        _ -> Brick.Main.continue (AISelectorMenu lst)

handelEvents state@State 
            { board = board
            , currPlayer = currPlayer
            , jumpingPiece = jumpingPiece
            , move = move
            , forcedMove = forcedMove
            , currCursorPos = (cursorY,cursorX)
            , aiOponnent = aiOponnent
            } e =
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
                $ state 
                { currCursorPos = ((cursorY-1) `mod` 8,cursorX) }

            x | x `elem` downs -> 
                Brick.Main.continue
                $ state 
                { currCursorPos = ((cursorY+1) `mod` 8,cursorX) }

            x | x `elem` rights -> 
                Brick.Main.continue
                $ state 
                { currCursorPos = (cursorY,(cursorX+1) `mod` 8) }

            x | x `elem` lefts -> 
                Brick.Main.continue
                $ state 
                { currCursorPos = (cursorY,(cursorX-1) `mod` 8) }

            V.KEnter -> 
                Brick.Main.continue
                . checkWinner
                . runAI
                . checkWinner
                . update
                $ state
            V.KEsc -> 
              Brick.Main.continue
              . (\s -> 
                  if forcedMove then
                    s
                  else
                    s {move = Nothing})
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
    if [] == validMoves board Black then
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
            , forcedMove = forcedMove
            , currCursorPos = currCursorPos@(cursorY,cursorX)
            , aiOponnent = aiOponnent
            } =
    case move of
        Nothing      -> 
            -- if color of piece == currPlayer then can move
            case board M.! upOne currCursorPos of
                Nothing -> state
                Just x  -> if colorOf x == currPlayer then
                               state {move = Just currCursorPos}
                           else
                               state
        Just (y1,x1) -> 
            let
              move :: ((Int,Int),(Int,Int))
              move = ((y1,x1),currCursorPos)
              wasJump :: Bool
              wasJump = abs (y1-fst currCursorPos) == 2
            in
                if move `elem` (validMoves board currPlayer) then
                    let
                        board' = applyMove move board
                    in
                        if  wasJump && canJump board' currCursorPos then
                            state { board      = board'
                                  , move       = Just currCursorPos 
                                  , forcedMove = True } 
                        else
                            state { board = board'
                                        , move       = Nothing
                                        , currPlayer = other currPlayer
                                        , forcedMove = False}
                else
                    state 
update x = x

validMove :: Board -> ((Int,Int),(Int,Int)) -> Bool
validMove board (m1,m2) =
    let
        (y1,x1) = upOne m1
        (y2,x2) = upOne m2
        (dy,dx) = (y2-y1,x2-x1)
        inBoard :: Int -> Bool
        inBoard x = 1 <= x && x <= 8
    in
        if dx==0 || dy==0 then
            False
        else if any (not . inBoard) [y1,x1,y2,x2] then
            False
        -- checkers can only move in a diagonal
        else if abs dx /= abs dy then
            False
        else case board M.! (y1,x1) of
                Nothing -> False
                Just piece ->
                    -- check that nothing is in the way
                    if Nothing /= board M.! (y2,x2) then
                       False 
                    -- check if can move in that direction
                    else if (case piece of
                                King _        -> False
                                NonKing color -> 
                                    case color of
                                        -- white checkers can only move up the board
                                        White -> dy>0
                                        -- black checkers can only move down the board
                                        Black -> dy<0) then
                        False 
                    -- can only move 2 if takeing a piece and if nothing is in the way
                    else if abs dx == 2 then
                        fromMaybe False
                        (do 
                            p <- board M.! (y1+(dy `div` 2),x1+(dx `div` 2))
                            return (colorOf p == (other . colorOf $ piece)))
                    else
                        True 

canJump :: Board -> (Int,Int) -> Bool
canJump board pos@(y,x) =
    any (validMove board)
    [(pos,(y+dy,x+dx)) | dx<-[-2,2] , dy<-[-2,2]]

validMoves :: Board -> Player -> [((Int,Int),(Int,Int))]
validMoves board player =
    concat
    . M.toList
    . M.mapPos 
        (\(maty,matx) piece -> 
            case piece of
                Nothing -> []
                Just piece  ->
                    let
                        (y,x) = (maty-1,matx-1)
                    in
                    if colorOf piece == player then
                        map (\a -> ((y,x),a))
                        [(y+dy,x+dx) 
                            | dx <- [-2..2] 
                            , dy <- [-2..2] 
                            , validMove board ((y,x),(y+dy,x+dx))] -- @TODO makable moves
                    else
                        [])
    $ board


applyMove :: ((Int,Int),(Int,Int)) -> Board -> Board
applyMove move board = -- @TODO
    let
        (y1,x1) = upOne . fst $ move
        (y2,x2) = upOne . snd $ move
        (dy,dx) = (y2-y1,x2-x1)
        piece   = board M.! (y1,x1)
        piece' = case colorOf <$> piece of
                  Just White | y2 == 1 -> Just $ King White
                  Just Black | y2 == 8 -> Just $ King Black
                  _ -> piece
        board'  = M.setElem piece' (y2,x2)
                . M.setElem Nothing (y1,x1)
                $ board
    in
    -- if jumoing over a piece makeshure the piece is taken
    if 2 == abs (x1-x2) then
        M.setElem Nothing (y1+(dy`div`2),x1+(dx`div`2)) board' 
    else
        board'
    

runAI :: State -> State
runAI = id

highlightShow :: Checker -> String
highlightShow (NonKing White) = "☻"
highlightShow (NonKing Black) = "☺"
highlightShow (King    White) = "♛"
highlightShow (King    Black) = "♕"



-- Instances
instance Show Checker where
    show (NonKing White) = "⬤"
    show (NonKing Black) = "◯"
    show (King White)    = "♚"
    show (King Black)    = "♔"
