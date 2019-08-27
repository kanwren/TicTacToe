module Main where

import Game (initialGame)
import Graphics.Gloss
import Logic (handleEvents)
import Rendering (gameToPicture, width, height)

title :: String
title = "Tic Tac Toe"

window :: Display
window = InWindow title (width, height) (10, 10)

bgColor :: Color
bgColor = white

main :: IO ()
main = play window bgColor 30 initialGame gameToPicture
    handleEvents (const id)
