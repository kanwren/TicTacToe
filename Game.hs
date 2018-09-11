{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens
import Data.Array

type Board = Array (Int, Int) Cell
type Cell = Maybe Player
data Player = PlayerX | PlayerO deriving Eq
data State = GameRunning | GameOver (Maybe Player) deriving Eq
data Game = Game { _board      :: Board
                 , _state      :: State
                 , _turn       :: Player
                 , _initPlayer :: Player
                 }
makeLenses ''Game

initialGame :: Game
initialGame = Game { _board      = emptyBoard
                   , _state      = GameRunning
                   , _turn       = PlayerX
                   , _initPlayer = PlayerX
                   }

emptyBoard :: Board
emptyBoard = listArray boardBounds $ repeat Nothing

boardMinIx, boardMaxIx :: Int
boardMinIx = -1
boardMaxIx = 1
boardBounds :: ((Int, Int), (Int, Int))
boardBounds = ((boardMinIx, boardMinIx), (boardMaxIx, boardMaxIx))
boardSpan :: [Int]
boardSpan = [boardMinIx..boardMaxIx]
boardDim :: Int
boardDim = boardMaxIx - boardMinIx + 1
