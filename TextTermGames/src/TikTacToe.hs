module TikTacToe where

import qualified Data.Vector as Vec
import qualified Data.Matrix as M
import qualified Graphics.Vty as V
import qualified Brick.Main
import qualified Brick.Types
import MinimaxLib
import System.Random (getStdRandom,randomR)
import Data.Maybe (fromMaybe,catMaybes,fromJust,isNothing)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Data.List (isInfixOf,intersperse,sortOn)
import Brick.Widgets.Border (borderWithLabel,border,hBorder,vBorder)
import Brick
import Brick.Widgets.List as BWList

-- For Debugging only
import Debug.Trace (trace)

-- A player can be O's or X's.
-- This is used to denote who's
-- turn it is and for the symbols
-- on the board.
data Player = O
            | X
            deriving (Eq,Show)

-- The board is a martix of X or O or Nothing
type Board = M.Matrix (Maybe Player)

-- Changes O to X and X to O
-- Isomorphic to not
other :: Player -> Player
other O = X
other X = O


-- The state of the game
-- State is a record that records game data
-- AISelectorMenu has a list to display at the
--     start of the game and select the AIoptions
-- Winner displays who the winner of the game is
--     and on any keybord input it sends you back 
--     to the main menu
data State 
    = State 
            { board         :: M.Matrix (Maybe Player)
            , currPlayer :: Player
            , currCursorPos :: (Int,Int)
            , aiOponnent :: Maybe Player
            }
    | AISelectorMenu (List String (Maybe Player))
    | Winner (Maybe Player)

-- Find all open spaces
--     The matrix uses (y,x) space
--     The list of open spaces are in (x,y) space
validMoves :: M.Matrix (Maybe Player) -> [(Int,Int)]
validMoves board
    = catMaybes
    . M.toList
    . M.mapPos (\(y,x) a -> case a of
        -- The matrix is one indexed
        -- but the location is zero indexed
        -- so we have to perform this switch
        Nothing -> Just (y-1,x-1) 
        -- If there is something there
        -- then the AI can't put something
        -- there
        Just _  -> Nothing)
    $ board
newStates :: Board -> Player
          -> [((Int,Int),Board)]
newStates board player = 
 [ (s,M.setElem (Just player) s board) 
   | s <- openSpaces board ]

openSpaces :: Board -> [(Int,Int)]
openSpaces board =
  map snd
  . filter (\(x,_) -> case x of
            Just _  -> False
            Nothing -> True)
  . M.toList
  . M.mapPos (\pos a -> (a,pos))
  $ board

miniMaxWithABD :: Board
               -> Player
               -> (Int,Int)
miniMaxWithABD board player =
  fromMaybe (-1,-1)
  . ((\(a,b) -> (a-1,b-1)) <$> ) -- translate the move from matrix
                                 -- space to screen space
  $ abMinimax terminalTest' score successors (board,player,player) where
    terminalTest' (board,turnPlayer,player) = terminalTest board
    successors (board,turnPlayer,player) = map (\(move,a) ->(move,(a,other turnPlayer,player)))
                                           $ newStates board turnPlayer
    score (board,turnPlayer,player) = 
      let
        openSpaces = foldr
          (\m i -> if isNothing m then i+1 else i)
          0
          (M.toList board)
      in
        case winner board of
          -- if a player can win in less 
          Just a | a == player -> 1+openSpaces
          Just a | a /= player -> -(1+openSpaces)
          Nothing              -> 0
    
  

terminalTest :: Board -> Bool
terminalTest board = isFull board || winner board /= Nothing

-- for testing purposes only
testBoard :: Board
testBoard = M.fromLists [ [ Just X, Nothing, Nothing]
                        , [ Just O, Just O, Nothing]
                        , [ Nothing, Nothing, Nothing]]


-- If there is a winner returns just the winner.
-- If there is no winner returns nothing.
-- use a magic square (http://mathworld.wolfram.com/MagicSquare.html) 
-- 1 for O 2 for X and 0 for blank.
-- if you sum any diaginal or row or colomn 
--     and get 15 O won if 30 X won.
winner :: M.Matrix (Maybe Player) -> Maybe Player
winner m =
    let
-- the magic square
        magicSquare :: M.Matrix Int
        magicSquare = M.fromList 3 3 
            [8,1,6
            ,3,5,7
            ,4,9,2]

        -- gets all of the prodct of all of the rows 
        checkRows :: M.Matrix Int -> [Int]
        checkRows m 
            = map sum
            . M.toLists
            $ m

        checkDiags :: M.Matrix Int -> [Int]
        checkDiags m = 
            [ M.trace m
            , M.trace . M.switchCols 1 3 $ m]

        overlayMatrix :: M.Matrix Int
        overlayMatrix 
            = M.elementwiseUnsafe (*) magicSquare
            . fmap (\x -> case x of
                Nothing -> 0
                Just O  -> 1
                Just X  -> 2)
            $ m

        checkedList :: [Int]
        checkedList = (checkRows overlayMatrix)
            ++(checkRows . M.transpose $ overlayMatrix)
            ++(checkDiags overlayMatrix)
    in
        case checkedList of
            x | 15 `elem` x -> Just O
            x | 30 `elem` x -> Just X
            _ ->  Nothing

-- If there are no open spaces then 
-- the board is full and no more moves
-- can be made.
isFull :: M.Matrix (Maybe Player) -> Bool
isFull m = Nothing `notElem` (M.toList m)

-- If the state is not State then act as the identity function.
-- If it is State then check for a winner
--      if there is a winner of the board is 
--          full then change to the Winner State
--      else return the state with no changes
-- Solution found: https://stackoverflow.com/questions/1056316/algorithm-for-determining-tic-tac-toe-game-over
checkWinner :: State -> State
checkWinner s@(Winner _) = s
checkWinner s@(AISelectorMenu _) = s
checkWinner s@(State 
           { board=board
           , currPlayer=_
           , currCursorPos=_
           , aiOponnent=_ }) 
        = case winner board of
            Nothing -> if isFull board then
                    Winner Nothing
                else
                    s
            x -> Winner x

-- If it is the AI's turn it takes a move
-- If a move is taken, move the cursor to where it took place
-- (this makes the AI feel like it is a player)
runAI :: State -> State
runAI state@(State 
        { board=board
        , currPlayer=currPlayer
        , currCursorPos=(locy,locx)
        , aiOponnent=ai }) = 
            case ai of
                Nothing -> state
                Just x | x /= currPlayer -> state
                Just x | x == currPlayer -> fromJust $ update $ State 
                    { board = board
                    , currPlayer = currPlayer
                    , currCursorPos = miniMaxWithABD board currPlayer
                    , aiOponnent = ai} 

runAI x = x

            
-- Returns the initial playable state
-- when given the AI selected
-- and runs the AI over the initial
-- state. 
initBoard :: Maybe Player -> State
initBoard x = runAI $ State 
    { board = M.matrix 3 3 (\_ -> Nothing)
    , currPlayer = O
    , currCursorPos = (0,0)
    , aiOponnent = x }

-- The initial state that is
-- a list of options for the
-- AI
init :: State
init = 
    let
        selectList = Vec.fromList [Nothing, Just X, Just O]
    in
        AISelectorMenu $ BWList.list "Tic-Tak-Toe-AI-List" selectList 2 

-- draw the state
draw :: State -> [Widget String] 
draw (AISelectorMenu lst) =
    let
        render :: Bool -> Maybe Player -> Widget String
        render b x = str $ (if b then "->" else "  ")
            ++ case x of
                Nothing -> " 2Player"
                Just X  -> " AI [X]"
                Just O  -> " AI [O]"
    in
        [BWList.renderList render True lst]
draw (Winner x) =
    let
        txt = case x of
            Nothing -> "Tie Game"
            Just a  -> "Congrats " ++ (show a)
    in
        [center . border . str $ txt]
draw State { board=board
           , currPlayer=currPlayer
           , currCursorPos=(locy,locx)
           , aiOponnent=_ }
    = 
    let 
        label = "Turn:"++(show currPlayer)
        rend :: Maybe Player -> String
        rend Nothing  = " "
        rend (Just p) = show p
    in
    [ center 
        . showCursor "use" (Location (2*locx+1,2*locy+1))
        . joinBorders
        . borderWithLabel (str label)
        . vBox 
        . map hBox
        . intersperse ( replicate 3 (hLimit 2 $ withBorderStyle unicode hBorder))
        . map (intersperse (vLimit 1 $ withBorderStyle unicode vBorder))
        . M.toLists
        . M.mapPos (\_ ->  str . rend)
        $ board ]

place :: M.Matrix (Maybe Player)
      -> Player
      -> (Int,Int)
      -> M.Matrix (Maybe Player)
place m p (y,x) =
    M.unsafeSet (Just p) (y+1,x+1) m

-- update the board and switch the player
-- based on the location of the cursor
update :: State -> Maybe State
update state@(State
           { board=board
           , currPlayer=currPlayer
           , currCursorPos=loc
           , aiOponnent=aiOponnent }) =
               case board M.! (1+fst loc,1+snd loc) of
                   Nothing -> (Just  State 
                    { board = place board currPlayer loc
                    , currPlayer = other currPlayer
                    , currCursorPos = loc
                    , aiOponnent = aiOponnent})

                   Just _ -> Nothing

update x = Just x



-- handelEvents 
-- handel the events for Tic-Tak-Toe
-- allows you to use the arrow keys and the vim keys
-- to move around
handelEvents :: State -> BrickEvent e () -> EventM e (Next State)
handelEvents (AISelectorMenu lst) event = case event of
        -- up 
    (VtyEvent (V.EvKey (V.KChar 'k') _)) -> Brick.Main.continue (AISelectorMenu $ listMoveUp lst)
    (VtyEvent (V.EvKey V.KUp _))         -> Brick.Main.continue (AISelectorMenu $ listMoveUp lst)
        -- down
    (VtyEvent (V.EvKey (V.KChar 'j') _)) -> Brick.Main.continue (AISelectorMenu $ listMoveDown lst)
    (VtyEvent (V.EvKey V.KDown  _))      -> Brick.Main.continue (AISelectorMenu $ listMoveDown lst)
        -- enter
    (VtyEvent (V.EvKey V.KEnter _))      -> (case listSelectedElement lst of
        Nothing -> Brick.Main.continue (AISelectorMenu lst)
        Just (_,option) -> Brick.Main.continue $ initBoard option
        )
    _ -> Brick.Main.continue (AISelectorMenu lst)


handelEvents state@(State 
           { board=board
           , currPlayer=currPlayer
           , currCursorPos=(x,y)
           , aiOponnent=aiOponnent }) event
    = 
    let
        ups = [((V.EvKey (V.KChar 'k') [])), ( (V.EvKey V.KUp []))]
        downs = [((V.EvKey (V.KChar 'j') [])), ( (V.EvKey V.KDown []))]
        lefts = [((V.EvKey (V.KChar 'h') [])), ( (V.EvKey V.KLeft []))]
        rights = [((V.EvKey (V.KChar 'l') [])), ((V.EvKey V.KRight []))]
        move :: (Int,Int) -> State
        move (x2,y2) = State 
            { board=board
            , currPlayer=currPlayer
            , currCursorPos= ((x+x2) `mod` 3,(y+y2) `mod` 3)
            , aiOponnent=aiOponnent }

    in case event of 
        
        VtyEvent x | x `elem` ups -> Brick.Main.continue $ move (-1,0)
        VtyEvent x | x `elem` downs ->Brick.Main.continue $ move (1,0)
        VtyEvent x | x `elem` lefts ->Brick.Main.continue $ move (0,-1)
        VtyEvent x | x `elem` rights ->Brick.Main.continue $ move (0,1)
        VtyEvent (V.EvKey V.KEnter []) -> Brick.Main.continue $
            checkWinner $ runAI $ checkWinner $ fromMaybe state (update state)
        _ -> Brick.Main.continue state
