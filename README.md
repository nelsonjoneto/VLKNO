# VLKNO

## 1. Group: VLKNO_8

**Members:**
- **Nelson José de Oliveira Neto**: 202108117
  - Contribution: 100% (Implemented the entire project)
- **Paulo Miguel de Jesus Coutinho dos Santos Fidalgo**: 201806603
  - Contribution: 0%

## 2. Installation and Execution

If you haven't installed SICStus Prolog 4.9, you may do so from the following link: [SICStus Prolog 4.9](https://sicstus.sics.se/).

### Linux
Clone the repository or download the zip of the code and extract it, navigate to the extracted folder and run the following commands in the terminal:
```bash
cd src
sicstus
[game].
play.
```

### Windows
Repeat the first step, open SICStus and using the menu consult `game.pl` and run `play.` in the SICStus terminal.

## 3. Description of the Game

VLKNO is a two-player abstract strategy game played on a 5x5 grid covered with 25 stackable stones. Each player controls two pawns of their color (black and white), which are initially placed on stones in diagonally opposite corners of the grid.

### Game Rules:

- **Setup**: Each player places their two pawns on stones in diagonally opposite corners of the grid.

- **Gameplay**: Players take turns performing the following actions:
  - **Move your Pawn** in any direction to a stack of stones that is the same height, one stone taller, or one stone shorter than the stack your pawn is currently on;
  - **Pick up a Stone**: Pick up a stone from the smallest unoccupied stack on the board, except the stack you just moved from. If there is a tie for the smallest stack, you choose which one to pick from;
  - **Place the Stone**: Place the stone on any unoccupied stack of 1+ stones, except the stack you just moved from.

- **Winning the Game**: The game ends when a player cannot complete each step of their turn. The other player is declared the winner.

### Links Used:
- [BoardGameGeek - VLKNO](https://boardgamegeek.com/boardgame/280615/vlkno)
- [Official Rulebook](https://nestorgames.com/rulebooks/VLKNO_EN.pdf)

## 4. Considerations for Game Extensions

When designing VLKNO, the primary consideration for game extensions was to allow for variable-sized boards. This enhancement aims to provide a more flexible and engaging gameplay experience by offering different board dimensions. Currently board sizes can vary between 4x4 and 8x8. The smaller sample size is due to AI performance issues for larger sized board, as any board of any size can be visually represented with no problems.

## 5. Game Logic

### Game Configuration Representation

The current game's configuration represents the players using the following structure: `Player1Type` and `Player2Type`, which indicate if the players are human or computer; `Player1Name` and `Player2Name`, which store the names of the players; `BoardSize`, which specifies the dimensions of the board; and `Difficulty1` and `Difficulty2`, which represent the difficulty levels for players (0 for humans; 1 and 2 for computers). These variables are used by the `initial_state/2` predicate to set up the initial game state, ensuring that all necessary information is available to start the game. The `Difficulty1` and `Difficulty2` variables are not stored in the game state and only used in the game_loop, but `Player1Type`, `Player2Type`, `Player1Name`, and `Player2Name` are stored as they are used throughout the game for display and moving purposes. The `BoardSize` variable is used by `initial_state/2` to create the board with dimensions BoardSize x BoardSize.

### Internal Game State Representation

The game state includes: the `CurrentPlayer`, which indicates whose turn it is (player1 or player2); the `Board`, which is a nested list representing the board where each cell contains the number of stones at that position; `P1C1` and `P1C2`, which are the positions of player 1's white pieces; `P2C1` and `P2C2`, which are the positions of player 2's black pieces; `Player1Type` and `Player2Type`, which indicate whether the players are human or computer; and `Player1Name` and `Player2Name`, which store the names of the players.

### Examples of Game State Representations

#### Initial State:
```
      1       2       3       4       5   
  -----------------------------------------
5 |  -W2- |  (1)  |  (1)  |  (1)  |  -B2- |
  -----------------------------------------
4 |  (1)  |  (1)  |  (1)  |  (1)  |  (1)  |
  -----------------------------------------
3 |  (1)  |  (1)  |  (1)  |  (1)  |  (1)  |
  -----------------------------------------
2 |  (1)  |  (1)  |  (1)  |  (1)  |  (1)  |
  -----------------------------------------
1 |  -B1- |  (1)  |  (1)  |  (1)  |  -W1- |
  -----------------------------------------
Stones at W1(5,1): 1
Stones at W2(1,5): 1
Stones at B1(1,1): 1
Stones at B2(5,5): 1
Current player: nelson
Choose a piece to move ([w1,w2]).
|: 
```

#### Intermediate Game State:
```
      1       2       3       4       5   
  -----------------------------------------
5 |  (1)  |  (1)  |  (3)  |  (3)  |  (1)  |
  -----------------------------------------
4 |       |  (1)  |  -W2- |       |  (1)  |
  -----------------------------------------
3 |       |       |  -B2- |  -W1- |  (1)  |
  -----------------------------------------
2 |  (3)  |       |  (1)  |       |  (1)  |
  -----------------------------------------
1 |  -B1- |       |  (1)  |  (2)  |  (1)  |
  -----------------------------------------
Stones at W1(4,3): 1
Stones at W2(3,4): 1
Stones at B1(1,1): 1
Stones at B2(3,3): 1
Current player: PC2
Hmmm... 
I think I'll move the piece b2 in direction down
I think I'll move a stone from (2,4) to (1,2)
Press any key to continue...
|:
```

#### Final Game State:
```
      1       2       3       4       5   
  -----------------------------------------
5 |  (1)  |  (1)  |  (3)  |  (3)  |  (1)  |
  -----------------------------------------
4 |       |       |  (1)  |       |  (1)  |
  -----------------------------------------
3 |       |       |  -W2- |  -W1- |  (1)  |
  -----------------------------------------
2 |  (4)  |       |  -B2- |       |  (1)  |
  -----------------------------------------
1 |  -B1- |       |       |  (3)  |  (1)  |
  -----------------------------------------
Stones at W1(4,3): 1
Stones at W2(3,3): 1
Stones at B1(1,1): 1
Stones at B2(3,2): 1
Current player: PC2
Game over! Winner: nelson
Do you want to play again? (yes/no): 
|:
```

**Legend:**
- **W1**: White piece 1
- **W2**: White piece 2
- **B1**: Black piece 1
- **B2**: Black piece 2
- **Stacks**: Number of stones in parentheses (e.g., (1)).
- **Lava**: Empty cell, unoccupiable.

### Move Representation

A move in VLKNO involves moving a piece and moving a stone. As such, moves are represented by a tuple containing the `Piece` being moved, the `Direction` of the move, the position from which a stone is picked up (`OldStonePos`), and the position where the stone is placed (`NewStonePos`). The possible values for `Piece` are `w1` (White piece 1), `w2` (White piece 2), `b1` (Black piece 1), and `b2` (Black piece 2). The `Direction` can be `up`, `down`, `left`, or `right`. The mentioned positions are represented as tuples `(Column, Row)`.

### Usage in move/3 Predicate

The `move/3` predicate uses this representation to update the game state. The predicate takes the current game state and the move to be executed. First it moves the `Piece` in the mentioned `Direction`, altering its position in the `GameState` and then it updates the `Board`, removing a stone from the `OldStonePos` and adding another to the `NewStonePos`. It then returns the `NewGameState` generated.

## User Interaction

The game menu system in VLKNO provides a user-friendly interface for players to configure and play the game. Interaction with the user is performed through prompts and input validations to ensure a smooth and error-free experience.

### Game Menu System

The game menu system includes several menus that guide the player through the game configuration and gameplay. In the **Main Menu**, players can select the game mode (e.g., Human vs Human, Human vs Computer) and enter player names. The **Difficulty Menu** allows players to choose the difficulty level for computer players (e.g., Easy, Hard). The **Board Size Menu** lets players choose the size of the board (4x4 to 8x8). During gameplay, prompts guide players to select a piece to move, specify the direction, and pick up and place stones on the board.

### Input Validation

Input validation ensures that players provide valid inputs, preventing errors and enhancing gameplay. Key points of validation include ensuring that the player selects a valid option from the menu, that player names are non-empty strings containing only letters and numbers with a maximum length of 20, and that the chosen board size is within the allowed range of 4x4 to 8x8.

### Move Validation

Move validation ensures that the selected piece belongs to the current player and has available moves. It validates that the piece move direction leads to another unoccupied position within board boundaries, where the stack height doesn't differ by more than one stone from the previous one. It also ensures that the stone is picked up from an unoccupied position that isn't the previous position occupied by the moved piece and has the minimum stack height on the board for available positions. Additionally, it checks that the target position for the piece is unoccupied by any pieces, isn't the position where the stone was removed from, and isn't the previous position occupied by the moved piece. By implementing these input validations, the game ensures that players provide valid inputs, preventing errors and enhancing the overall gameplay experience.

## Computer Player Algorithms and Game State Evaluation

### Difficulty Levels

The game includes different difficulty levels for computer players, each implementing distinct algorithms to determine moves. In **Easy Difficulty**, the computer player selects a move randomly from the list of valid moves. In **Hard Difficulty**, the computer player uses a greedy algorithm to select the move with the highest immediate value.

### Game State Evaluation

The game state evaluation is a crucial component of the AI's decision-making process, especially for the hard difficulty level. The evaluation function assigns a value to each potential game state, allowing the AI to compare and select the best move.

**Evaluation Criteria**: The evaluation criteria include the difference in the number of available piece moves and the difference in reachable positions. The difference of available piece moves evaluates the number of valid moves available to each player's pieces, with a higher number of moves for the AI's pieces and fewer for the opponent's pieces being advantageous. The difference of reachable positions assesses the number of positions that each player's pieces can reach, with positions providing strategic advantages, such as controlling key areas of the board or limiting the opponent's mobility, being given higher value. The evaluation function, `value`, combines these criteria into a single value, representing the desirability of a game state. The AI uses this value to rank potential moves and select the one with the highest value.

## 8. Conclusions

The development of VLKNO provided valuable insights into the design and implementation of a strategic board game using Prolog. The project successfully implemented the core game mechanics, including piece movement, stone placement, and AI decision-making for different difficulty levels. The game offers a flexible and engaging experience with variable-sized boards and a user-friendly interface.

### Limitations

**AI Performance**: The AI's performance the higher difficulty level on larger board sizes can lead to high waiting times between moves, despite the implemented optimization.

### Possible Improvements

Possible improvements for VLKNO include finding a better and more efficient AI solution for larger board sizes, allowing the user to cancel their move if they mistype it to provide a more user-friendly experience and reduce frustration, introducing new game modes and optional rules to provide more variety and replayability, and implementing the game using sockets to enable gameplay on separate machines.

## 9. Bibliography

- [SICStus Prolog Documentation (HTML)](https://sicstus.sics.se/sicstus/docs/latest4/html/sicstus.html/)
- [SICStus Prolog Documentation (PDF)](https://sicstus.sics.se/sicstus/docs/latest4/pdf/sicstus.pdf)
