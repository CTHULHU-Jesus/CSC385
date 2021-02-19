module Connect4 where

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


data Player
        = White
        | Black
        deriving (Eq,Show)

data State 
        = AISelectorMenu (BWList.List String (Maybe Player))
        | Winner (Maybe Player)
        | State 
            { board :: M.Matrix (Maybe Player)
            , currPlayer :: Player
            , currCursorPos :: Int
            , aiOponnent :: Maybe Player
            }

-- Copied from https://stackoverflow.com/a/32466695
diagonals :: [[a]] -> [[a]]
diagonals []       = []
diagonals ([]:xss) = xss
diagonals xss      = zipWith (++) (map ((:[]) . head) xss ++ repeat [])
                                  ([]:(diagonals (map tail xss)))


allSubLines :: M.Matrix (Maybe Player)
            -> [[Maybe Player]]
allSubLines m =
    let
        rows    :: [[Maybe Player]]
        rows = M.toLists m

        cols    :: [[Maybe Player]]
        cols = M.toLists 
             . M.transpose 
             $ m

        diag    :: M.Matrix (Maybe Player)
                -> [[Maybe Player]]
        diag m = (\l -> drop 3 
                      . reverse
                      . drop 3
                      $ l)
               . diagonals 
               . M.toLists 
               $ m

        offDiag :: [[Maybe Player]]
        offDiag = diag
                $ M.matrix 6 7 (\(y,x) -> m M.! (y,7-x+1))
    in
        rows ++ cols ++ (diag m) ++ offDiag

winner :: M.Matrix (Maybe Player)
       -> Maybe Player
winner board =
    let
        isWinner :: Player
                 -> Bool
        isWinner p = foldl (||) False
                   . map ((replicate 4 (Just p)) `isInfixOf`)
                   $ allSubLines board
    in
        if isWinner White then
            Just White
        else if isWinner Black then
            Just Black
        else
            Nothing

openSpaces :: M.Matrix (Maybe Player) -> [Int]
openSpaces m = [x | x<-[0..6] ,
                    Nothing `Vec.elem` (M.getCol (x+1) m)]



-- Minimax algorithm with alpha beta pruning and depth stop
miniMaxWithABD :: M.Matrix (Maybe Player)
               -> Player
               -> Int
miniMaxWithABD board player =
    let
        maxDepth :: Int
        maxDepth = 9

        helper :: M.Matrix (Maybe Player)
               -> Player
               -> Int
               -> Int
               -> Int
               -> (Int,Int) -- (move,score)
        helper board me depth alpha beta = 
            let
                dive :: Int
                     -> Int
                     -> [(Int,M.Matrix (Maybe Player))]
                     -> [(Int,Int)]
                dive _ _ [] = []
                dive alpha beta ((move,board):xs) = (move,score):more where
                    score :: Int
                    score = (snd 
                          $ helper board (other me) (depth+1) alpha beta)
                    more  = case (me == player) of
                            -- maximizing
                        True -> if score >= beta then
                                    []
                                else
                                    dive (max alpha score) beta xs
                            -- minimizing
                        False -> if score <= alpha then
                                     []
                                 else
                                     dive alpha (min beta score) xs

                buildBoards :: [Int]
                            -> [(Int,M.Matrix (Maybe Player))]
                buildBoards = map (\x -> (x,place board me x))

                evaluate :: Int
                                -- other me is used here because that is what
                                -- helper is called with
                evaluate = (if (other me) == player then 1 else -1) *
                           (case winner board of
                                Just _  -> 100-depth
                                Nothing -> 0)
            in
                if depth == maxDepth then
                    (0,evaluate)
                else if (winner board) /= Nothing then
                    (0,evaluate)
                else if isFull board then
                    (0,0)
                else
                   (if me == player then head else last)
                   . sortOn snd
                   . dive alpha beta
                   . buildBoards
                   $ openSpaces board
    in
        fst 
        $ helper board player 0 (minBound :: Int) (maxBound :: Int)


runAI :: State
      -> State
runAI state@(State
    {board=board
    ,currPlayer=currPlayer
    ,currCursorPos=currCursorPos
    ,aiOponnent=aiOponnent}) 
    = case aiOponnent of
        Nothing                  -> state
        Just x | x /= currPlayer -> state
        Just x | x == currPlayer -> fromJust
                                  . update
                                  . (\x -> upCurrCursorPos x state)
                                  $ miniMaxWithABD board currPlayer

runAI x = x

isFull :: M.Matrix (Maybe Player)
       -> Bool
isFull m = Nothing `notElem` m

checkWinner :: State 
            -> State
checkWinner state@(State
    {board=board
    ,currPlayer=currPlayer
    ,currCursorPos=currCursorPos
    ,aiOponnent=aiOponnent}) =
        case winner board of
            Just x -> Winner (Just x)
            Nothing ->  if isFull board then
                    Winner Nothing
                else
                    state
        
checkWinner x = x


other :: Player -> Player
other White = Black
other Black = White

upBoard :: M.Matrix (Maybe Player) -> State -> State
upBoard board
    (State
    {board=_
    ,currPlayer=currPlayer
    ,currCursorPos=currCursorPos
    ,aiOponnent=aiOponnent}) =
        State 
        { board         = board
        , currPlayer    = currPlayer
        , currCursorPos = currCursorPos
        ,aiOponnent     = aiOponnent
        }
upBoard _ x = x

upCurrPlayer :: Player -> State -> State
upCurrPlayer currPlayer
    State
    {board=board
    ,currPlayer=_
    ,currCursorPos=currCursorPos
    ,aiOponnent=aiOponnent} =
        State 
        { board         = board
        , currPlayer    = currPlayer
        , currCursorPos = currCursorPos
        ,aiOponnent     = aiOponnent
        }
upCurrPlayer _ x = x

upCurrCursorPos :: Int -> State -> State
upCurrCursorPos currCursorPos
    State
    {board=board
    ,currPlayer=currPlayer
    ,currCursorPos=_
    ,aiOponnent=aiOponnent} =
        State 
        { board         = board
        , currPlayer    = currPlayer
        , currCursorPos = currCursorPos
        ,aiOponnent     = aiOponnent
        }
upCurrCursorPos _ x = x




initState :: State
initState = 
    let
        selectList = Vec.fromList [Nothing, Just White, Just Black]
    in
        AISelectorMenu $ BWList.list "Connect4-AI-List" selectList 2 


initBoard :: Maybe Player -> State
initBoard aiOponnent = runAI $ State
    { board         = M.matrix 6 7 (\_ -> Nothing)
    , currPlayer    = White
    , currCursorPos = 0
    , aiOponnent    = aiOponnent
    }


draw :: State -> [Widget String] 
draw (AISelectorMenu lst) =     
    let
        render :: Bool -> Maybe Player -> Widget String
        render b x = str $ (if b then "->" else "  ")
            ++ case x of
                Nothing -> " 2Player"
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
                Just x  -> "Congrats " ++ (show x)
    in
        [ center 
        . border 
        $ str txt]

draw state@(State 
            { board = board
            , currPlayer = currPlayer
            , currCursorPos = pos
            , aiOponnent = aiOponnent
            }) =
        let 
            label :: String
            label = (show currPlayer) ++ "'s Turn"

            rend :: Maybe Player -> String
            rend p = case p of
                Nothing    -> " "
                Just White -> "⬤"
                Just Black -> "◯"

            bottomRow :: [[Widget String]]
            bottomRow = [
                      replicate 7
                    . hLimit 1 
                    $ withBorderStyle unicode hBorder
                    , map ( str . show)
                    $ [1..7]
                ]
                
        in
            [ center 
            . showCursor "use" (Location (2*pos+1,8))
            . joinBorders
            . borderWithLabel (str label)
            . vBox 
            . map hBox
            . map (intersperse (vLimit 1 $ withBorderStyle unicode vBorder))
            . (++ bottomRow)
            . M.toLists
            . M.mapPos (\_ ->  str . rend)
            $ board ]


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

handelEvents state@(State 
            { board = board
            , currPlayer = currPlayer
            , currCursorPos = pos
            , aiOponnent = aiOponnent
            }) event =
    case event of
        (VtyEvent (V.EvKey key _)) -> (case key of
                -- left
            x | x `elem` [V.KChar 'h',V.KLeft] ->  
                Brick.Main.continue $ upCurrCursorPos ((pos-1) `mod` 7) state
                -- right
            x | x `elem` [V.KChar 'l',V.KRight] ->  
                Brick.Main.continue $ upCurrCursorPos ((pos+1) `mod` 7) state
                -- select @TODO
            V.KEnter ->  
                Brick.Main.continue 
                . checkWinner
                . runAI
                . checkWinner
                . fromMaybe state
                $ update state
            _ -> Brick.Main.continue state
            )
        _ -> Brick.Main.continue state

place :: M.Matrix (Maybe Player)
      -> Player
      -> Int
      -> M.Matrix (Maybe Player)
place m p pos =
    let
        col :: [Maybe Player]
        col = Vec.toList $ M.getCol (pos+1) m

        f :: [Maybe Player] -> Int
        f [] = 0
        f (Just _:_) = 0
        f (Nothing:xs) = 1+(f xs)
    in
        M.setElem (Just p) (f col,pos+1) m


-- @TODO
update :: State -> Maybe State
update state@(State 
            { board = board
            , currPlayer = currPlayer
            , currCursorPos = pos
            , aiOponnent = aiOponnent
            }) = 
    let
        col :: [Maybe Player]
        col = Vec.toList $ M.getCol (pos+1) board
    in
        case Nothing `notElem` col of
            -- if there is nothing to add return nothing
            True -> Nothing
            False -> 
                let
                    f :: [Maybe Player] -> Int
                    f [] = 0
                    f (Just _:_) = 0
                    f (Nothing:xs) = 1+(f xs)
                in
                    Just
                    . upCurrPlayer (other currPlayer)
                    . upBoard (place board currPlayer pos)
                    $ state
update x = Just x
