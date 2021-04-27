---
title: Game AI for Simple Board Games
author: Matthew Bartlett
theme: Copenhagen
date:
lang: en-US
section-titles: false
---

# Project Overview
- Have 3 text terminal games playable with AI
- Games:
  * Tic-Tak-Toe
  * Connect 4
  * Checkers
  * ~~Chess~~

# MiniMax (Basic)
- We have a game with two sides that go back an forth.
- We have some function that says how good a game state is
  * Large if it is good for us (AI)
  * Small if it is good for our opponent
- We recur down the tree of possible moves/game states
  * If it is our opponents turn we pick minimise the score
  * If it is our turn we take the move with maximum score.
  * If someone wins or we have reached our maximum recursion depth return the score for the current game state


# The Hard Part
- Selecting a function to evaluate game state
- **IF** you can explore the entire game state space quickly enough, you can just select the move where you can win in the fewest moves
- I was able to do this for Tic-Tak-Toe
- For most games you can't do this

# Connect 4
- $player\_score(b,p)=$number of $2^s$ in a row $+ 3*$number of $3^s$ in a row, for player $p$ in board $b$
- Then the total score is 
  * 1000- turns taken, if it is a winning board
    * negative if the other player won
  * $player\_score(b,p)-player\_score(b,other(p))$
    * $b$ is the current board
    * $p$ is the player we are optimising for

# Checkers
- $player\_score(b,p)=$add up the score of all of player $p$'s pieces, 1 for normal checkers 4 for kings in board $b$
- Alternatively we could optimize for the number of moves we can make because you lose checkers when you can't make any more moves
- The total score should look familiar
  * 1000 if it is a winning board
    * negative if the other player won
  * $player\_score(b,p)-player\_score(b,other(p))$
    * $b$ is the current board
    * $p$ is the player we are optimising for


