module Main where

import Act1

main :: IO ()
main = do
  -- print intro and instructions from Act1
  printLines introductionText
  printLines instructionsText

  -- show starting location
  printLines [ describeLocation (currentLocation initialState), "" ]

  -- hand off into the Act1 game loop
  gameLoop initialState
