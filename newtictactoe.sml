use "utils.sml";
use "matrix.sml";

signature TTTSTATE = sig

  structure Matrix : SQUAREMATRIX

  datatype cell
    = Empty
    | X
    | O

  type state = cell * (cell Matrix.matrix)

  type effect = Matrix.index

  val tranFunc : effect -> state -> state

  val isEmpty : cell -> bool

end

functor TttStateFn (M : SQUAREMATRIX) : TTTSTATE =
  struct

  structure Matrix = M

  datatype cell
    = Empty
    | X
    | O

  type state = cell * (cell Matrix.matrix)

  type effect = Matrix.index

  fun tranFunc i (X,board) = (O, Matrix.update (board,i,X))
    | tranFunc i (O,board) = (X, Matrix.update (board,i,O))

  fun isEmpty Empty = true
    | isEmpty _     = false

  end
