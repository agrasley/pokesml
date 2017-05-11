# SML Game Runner and Tic Tac Toe Implementation
We built a generic game runner and implemented tic tac toe in it.

## Running

To run the tic tac toe implementations, first load the code:

```sml
use "tictactoe.sml";
```

Then, choose one the following modules:

```sml
TttExecHuman (* human players on 2D board *)
TttExec3DHuman (* human players on 3D board *)
TttExecRandom (* random AI players on 2D board *)
TttExec3DRandom (* random AI players on 3D board *)
```

Then call:

```sml
ChosenModule.run i; (* where i is an integer designating board size *)
```

For example, to play a classic game of tic tac toe between two people on a 3X3
grid you would call:

```sml
TttExecHuman.run 3;
```

This should then display:

```
Current State:

---------
|0||1||2|
---------
|3||4||5|
---------
|6||7||8|
---------

Please enter the number of the cell you want to fill:

```

From there, you simply input the number corresponding to the cell you want to fill
and hit enter. The game ends when the board is full.

You can also run one of the random modules and see how the game plays out with
no human input.

## Brief Code Tour

### game.sml

This is our generalized game runner. It defines modules that can be used to run
a simple game. A game is broken down into a state, actions on that state, agents
that produce actions, and a functor that wraps them all together into something
runnable.

The signatures are deliberately left as general and minimal as possible in
order to allow for different implementations. While we chose tic tac toe as an
easy to implement case study, you could feasibly implement a number of games and
still use the same ExecFn functor to run the implementation.

### tictactoe.sml

Here we implement our abstract game runner using tic tac toe. Many of the signatures
defined in game.sml are refined here to work specifically for tic tac toe. Our
implementation uses functors to allow for generalized tic tac toe using n-dimensional
boards and both human and AI players.

### matrix.sml

Defines the matrix ADT used to simulate a tic tac toe board. Matrices are built
using the CONTAINER signature, which generalizes containers. Using functors, it
is possible to build matrices out of a number of data structures, including lists,
vectors, and arrays. It's even possible to mix several of these data structures
together to create a matrix. Additionally, it is possible to build n-dimensional
square matrices using the SquareMatrixFn functor.

### utils.sml

Some useful function, many of which are stolen from Haskell.
