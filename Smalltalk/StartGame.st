"Load game files in correct order"
FileStream fileIn: 'GameUtils.st'!
FileStream fileIn: 'Act1.st'!
FileStream fileIn: 'Act2.st'!
FileStream fileIn: 'Act3.st'!
FileStream fileIn: 'Main.st'!

"Initialize classes"
EntityType initialize!
Location initialize!

"Start the game"
'Starting The Hidden Realm...' displayNl!
GameMain startGame!
