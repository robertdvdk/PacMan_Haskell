# PacMan_Haskell
A Pac-Man implementation in Haskell by Robert van der Klis and Jurre Luijten.

Use "cabal build" to build the game when running for the first time. Afterwards, use "cabal run" to run the game.

Control Pac-Man and gather points by eating the dots in the maze. Small dots give 10 points, and big dots give 50 points and make the ghosts vulnerable. You get three lives, and if you lose a life the level gets reset to its initial state. There are three levels, and each next level is harder than the previous one: the first has 2 ghosts, the second has 3, and the third has 4 ghosts. You advance to the next level by eating all the dots in the current level. 

Keybinds: "WASD" for movement, and "P" to pause or resume the game.
A few keybinds are added in for testing: "T" to advance to the next level, and "G" to trigger the "Game Over" state.

Have fun!