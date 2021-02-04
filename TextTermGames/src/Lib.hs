module Lib where

-- Imports

import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Brick.Main
import qualified TikTacToe
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel)
import Brick
import Brick.Widgets.List as BWList

-- Types

data Games
        = TopMenu (BWList.List String String)
        | TikTacToe TikTacToe.State
        | Connect4 ()
        | Checkers ()
        | Chess ()

-- Constants 

menuItems = 
        [ "Tik-Tac-Toe"
        , "Chess"
        , "Checkers"
        , "Connect4"
        , "Exit" ]

initalState = TopMenu $
                    BWList.list "Game Menu" 
                        (Vec.fromList menuItems) 
                        (length menuItems)
    

-- Functions


noMap :: Games -> AttrMap 
noMap _ = attrMap V.defAttr []


-- Gives the name of the Game
name :: Games -> String
name (TikTacToe _) = "Tik-Tac-Toe"
name (Chess     _) = "Chess"
name (Checkers  _) = "Checkers"
name (Connect4  _) = "Connect4"
name (TopMenu   _) = "Top Menu"

-- If Nothing then display the game select screen
menu :: Games -> [Widget String]
menu (TikTacToe state) = TikTacToe.draw state
menu (Chess ())        = []
menu (Checkers ())     = []
menu (Connect4 ())     = []
menu (TopMenu menuList)= 
    let
        rend :: Bool -> String -> Widget String
        rend b game = str $ ( if b then "->" else "  ") ++ (game)
    in
        [ borderWithLabel (str "Game Select Menu") $ 
            BWList.renderList rend True menuList ]

-- handel the events of the Games menu and if it is not on the menu pass the event
-- to the indivitual game handelers
-- Exeptions (events that are only handeld here) :
--  Ctrl+c
handelEvents :: Games -> BrickEvent e () -> EventM e (Next Games)
handelEvents state (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = Brick.Main.halt state
handelEvents (TopMenu menuList) event =
    case event of
        -- up
        (VtyEvent (V.EvKey (V.KChar 'k') _)) -> Brick.Main.continue (TopMenu $ listMoveUp menuList)
        (VtyEvent (V.EvKey V.KUp _))         -> Brick.Main.continue (TopMenu $ listMoveUp menuList)
        -- down
        (VtyEvent (V.EvKey (V.KChar 'j') _)) -> Brick.Main.continue (TopMenu $ listMoveDown menuList)
        (VtyEvent (V.EvKey V.KDown  _))      -> Brick.Main.continue (TopMenu $ listMoveDown menuList)
        -- enter
        (VtyEvent (V.EvKey V.KEnter _))      -> (case listSelectedElement menuList of
                                        Just (_,"Exit") -> Brick.Main.halt (TopMenu menuList)
                                        Just (_,"Tik-Tac-Toe") -> Brick.Main.continue $ 
                                            TikTacToe TikTacToe.init
                                        Just (_,"Connect4") -> Brick.Main.continue $
                                            Connect4 ()
                                        Just (_,"Checkers") -> Brick.Main.continue $
                                            Checkers ()
                                        Just (_,"Chess")    -> Brick.Main.continue $
                                            Chess ()
                                        _ -> Brick.Main.continue (TopMenu menuList)
            )
handelEvents state@(TikTacToe (TikTacToe.Winner _)) e = case e of
	VtyEvent _ -> Brick.Main.continue initalState
	_ -> Brick.Main.continue state
handelEvents (TikTacToe state) e = fmap (fmap TikTacToe) $ TikTacToe.handelEvents state e
-- Instances
