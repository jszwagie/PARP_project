| files |
files := #(
    'entity.st'       
    'base_game.st'    
    'act1.st'
    'act2.st'
    'act3.st'
    'main_game.st'
).
files do: [:each | FileStream fileIn: each ].
MainGame run.