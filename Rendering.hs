module Rendering where

import Control.Lens
import Data.Array (assocs)
import Game
import Graphics.Gloss

grid :: Picture
grid = color black $ pictures $ concatMap genLinePair ticks
    where ticks = [boardMinIx-1..boardMaxIx+1]
          screenH = fromIntegral height / 2
          screenW = fromIntegral width / 2
          genLinePair tick = let distX = (fromIntegral tick + 0.5) * cellWidth
                                 distY = (fromIntegral tick + 0.5) * cellHeight
                             in [line [(distX, -screenH), (distX, screenH)],
                                 line [(-screenW, distY), (screenW, distY)]]

width, height :: Int
width = 600
height = 600
cellWidth, cellHeight, minDim :: Float
cellWidth = fromIntegral width / fromIntegral boardDim
cellHeight = fromIntegral height / fromIntegral boardDim
minDim = min cellWidth cellHeight

xPicture, oPicture :: Picture
xPicture = let barWidth = 10.0
               barHeight = minDim * 0.75
               xBar = rectangleSolid barWidth barHeight
           in pictures [rotate 45 xBar, rotate (-45) xBar]
oPicture = thickCircle radius thickness
    where radius = minDim * 0.75 / 2 - thickness * 2
          thickness = 10.0

playerToPicture :: Player -> Picture
playerToPicture PlayerX = xPicture
playerToPicture PlayerO = oPicture

xoToCoord :: (Int, Int) -> Picture -> Picture
xoToCoord coord = uncurry translate (coord & both %~ ixToDistFromCenter)
    where ixToDistFromCenter n = fromIntegral n * minDim

pictureWhenRunning game = let
    as = assocs $ game ^. board
    xPictures = pictures [xoToCoord c xPicture | (c, Just PlayerX) <- as]
    oPictures = pictures [xoToCoord c oPicture | (c, Just PlayerO) <- as]
    in pictures [color red xPictures, color blue oPictures]

colorWhenOver Nothing = greyN 0.5
colorWhenOver (Just PlayerX) = red
colorWhenOver (Just PlayerO) = blue

pictureWhenOver game winner = let
    gameBoard = game ^. board
    playerPics = [(i, playerToPicture e) | (i, Just e) <- assocs gameBoard]
    xoPics = pictures $ map (uncurry xoToCoord) playerPics
    in color (colorWhenOver winner) xoPics

gameToPicture :: Game -> Picture
gameToPicture game = pictures [grid, xoPicture]
    where xoPicture = case game ^. state of
            GameRunning -> pictureWhenRunning game
            GameOver winner -> pictureWhenOver game winner
