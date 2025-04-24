module Main where

import Act1 (startAct1)
import Act2 (startAct2)
import Utils (Lines, emptyPlayer, printLines)

introductionText :: Lines
introductionText =
  [ "INTRODUCTION:",
    "Welcome to \"The Hidden Realm\", an interactive fiction adventure",
    "set in the icy depths of Antarctica. Unravel ancient secrets,",
    "face modern dangers, and shape your fate through your choices.",
    "Don't be afraid to use HINTS - they will help you navigate",
    "challenges and uncover the truth. Stay sharp - the unknown awaits.",
    ""
  ]

main :: IO ()
main = do
  printLines introductionText
  ps1 <- startAct1 emptyPlayer
  ps2 <- startAct2 ps1
  return ()
