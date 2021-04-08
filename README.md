# CSC385: A Collection of Text-Terminal games

## Specification
* Have 4 text terminal games written in haskell that all have competent AI opponents
- Tik-Tac-Toe 
- 4 in a Row
- Checkers
- Chess

## Configuration instructions
- Have stack installed
  - Install from: [Stack](https://docs.haskellstack.org/en/stable/README/)
## Installation instructions
- To play the games go to the "TextTermGames" directory and run with "stack run"
- If you do not have stack installed this will no work.

## Operating instructions
- Some notes on the controls
  - [Ctrl+c] will exit the program at any point
  - TicTacToe
    - You can select to play an AI
    - The 'O' player always goes first
  - Connect4
    - White player always goes first
  - Checkers
    - White always goes first
    - If you want to cancel selecting a piece hit [Esc]
    - If you can't cancel a move it is because the rules of Checkers dictate that you MUST make take another piece if you can.

## A file manifest

## Known bugs
* In Tic-Tac-Toe the minimax algorithm does not function as it should
## Credits and acknowledgements
* Example for using the minimax algorithm
- https://www.geeksforgeeks.org/minimax-algorithm-in-game-theory-set-3-tic-tac-toe-ai-finding-optimal-move/
