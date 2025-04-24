module Act2 (startAct2) where

import Utils (Lines, printLines, printPrompt, readCommand)

act2Prolog :: Lines
act2Prolog =
  [ "ACT 2: DESCENT INTO THE UNKNOWN",
    "You are sitting in the co-pilot chair of a Douglas A-20 Havoc, soaring over",
    "Antarctica's icy expanse. Clara pilots beside you, and you hold Admiral Byrd's diary."
  ]

startAct2 :: IO ()
startAct2 = do
  printLines act2Prolog

