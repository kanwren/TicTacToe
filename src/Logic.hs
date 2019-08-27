module Logic where

import Control.Lens
import Data.Array
import Data.Maybe (isJust, isNothing)
import Game
import Graphics.Gloss.Interface.Pure.Game
import Rendering (width, height, cellWidth, cellHeight)

screenToBoardCoord :: (Float, Float) -> (Int, Int)
screenToBoardCoord (x, y) = let
    x' = (x + fromIntegral width / 2) / cellWidth
    y' = (y + fromIntegral height / 2) / cellHeight
    readjust = floor . (+fromIntegral boardMinIx)
    in (readjust x', readjust y')

nextPlayer :: Player -> Player
nextPlayer player = case player of
    PlayerX -> PlayerO
    PlayerO -> PlayerX

boardFull :: Board -> Bool
boardFull = all isJust . elems

playerWon :: (Int, Int) -> Player -> Board -> Bool
playerWon (x, y) player prevBoard = let
       row      = [prevBoard ! (i, y)        | i <- boardSpan]
       col      = [prevBoard ! (x, i)        | i <- boardSpan]
       diag     = [prevBoard ! (i, i)        | i <- boardSpan]
       antiDiag = [prevBoard ! (negate i, i) | i <- boardSpan]
       wholeRow = all (==Just player)
    in any wholeRow [row, col, diag, antiDiag]

nextState :: (Int, Int) -> Player -> Board -> State
nextState coord lastPlayer newBoard
    | playerWon coord lastPlayer newBoard = GameOver (Just lastPlayer)
    | boardFull newBoard = GameOver Nothing
    | otherwise = GameRunning

placeXO :: (Int, Int) -> Game -> Game
placeXO coord game = let
    currentPlayer = game ^. turn
    currentBoard = game ^. board
    newBoard = currentBoard // [(coord, Just currentPlayer)]
    in game & board .~ newBoard & turn %~ nextPlayer
            & state .~ nextState coord currentPlayer newBoard

validBoardCoord :: (Int, Int) -> Bool
validBoardCoord = inRange boardBounds

isAvailable :: (Int, Int) -> Game -> Bool
isAvailable c game = let gameBoard = game ^. board in isNothing $ gameBoard ! c

handleEvents (EventKey (MouseButton LeftButton) Up _ mouse) game
    | game ^. state /= GameRunning = let p = nextPlayer (game ^. initPlayer)
        in game & board      .~ emptyBoard
                & turn       .~ p
                & state      .~ GameRunning
                & initPlayer .~ p
    | otherwise = if validBoardCoord boardCoord && isAvailable boardCoord game
                    then placeXO boardCoord game else game
    where boardCoord = screenToBoardCoord mouse
handleEvents _ game = game
