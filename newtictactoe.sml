use "utils.sml";
use "matrix.sml";

signature TTTSTATE = sig

  structure Matrix : SQUAREMATRIX

  datatype cell
    = Empty
    | X
    | O

  type state = cell * (cell Matrix.container)

  type effect = Matrix.index

  val tranFunc : effect -> state -> state

  val isEmpty : cell -> bool

  val showState : state -> string

end

functor TttStateFn (M : SQUAREMATRIX) : TTTSTATE =
  struct

  structure Matrix = M

  datatype cell
    = Empty
    | X
    | O

  type state = cell * (cell Matrix.container)

  type effect = Matrix.index

  fun tranFunc i (X,board) = (O, Matrix.update (board,i,X))
    | tranFunc i (O,board) = (X, Matrix.update (board,i,O))

  fun isEmpty Empty = true
    | isEmpty _     = false

  fun showCell X     = "|X|"
    | showCell O     = "|O|"
    | showCell Empty = "| |"


  fun showState (_,mat) = let
                            val s = Matrix.size mat
                            val l = implode (List.tabulate (s*3,fn _ => #"-"))
                            val f = fn (c, (acc, i)) => if i mod (s*s) = 0 then
                                (acc ^ "\n" ^ l ^ "\n" ^ showCell c, i+1)
                              else if i mod s = s-1 then
                                (acc ^ showCell c ^ "\n" ^ l ^ "\n", i+1)
                              else (acc ^ showCell c, i+1)
                          in
                            fst (Matrix.foldl f ("",0) mat)
                          end

  end

structure TttState = TttStateFn(ArrayMatrix)
structure Ttt3DState = TttStateFn(Array3DMatrix)
