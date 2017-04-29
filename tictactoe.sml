open "game.sml"

(************************ Sample tic tac toe environment ***********************)
signature MATRIX =
sig
    type ''a row = ''a vector
    type ''a col = ''a vector

    type 'a matrix = 'a row col
    exception OutOfBounds

    type index
    type dimensions = index * index
    type size = index * index

    val lookup : index * index -> 'a matrix -> 'a
    val fromList : 'a list list -> 'a matrix option

    val replace : index * index -> 'a -> 'a matrix -> 'a matrix
    val init : size -> 'a -> 'a matrix
    (* val getRow : index -> 'a matrix -> 'a row option *)
    (* val getCol : index -> 'a matrix -> 'a list option *)
end

structure Matrix : MATRIX =
struct

  type ''a row = ''a vector
  type ''a col = ''a vector
  type 'a matrix = 'a row col

  type index = int
  type dimensions = index * index
  type size = index * index

  exception OutOfBounds

  open Vector

  fun lookup (i, j) xs =
      let val x = Vector.sub (xs, i)
      in Vector.sub (x, j)
      end

  fun fromList nil = NONE
    | fromList (xs : 'a list list) =
      SOME (Vector.map Vector.fromList (Vector.fromList xs))

  fun replace (i, j) elem matrix =
    let val newrow = Vector.update (Vector.sub (matrix, i), j, elem)
    in Vector.update (matrix, i, newrow)
    end

  fun init (i, j) seed =
    let fun mkRow _ = List.tabulate (j, fn _ => seed)
    in valOf (fromList (List.tabulate (i, mkRow)))
    end
end

structure tttState :> STATE =
struct

datatype cell
  = Empty
  | X
  | O

(* should this be a functor? *)
structure M = Matrix

type state = cell M.matrix

datatype effects
  = Place of cell * int * int

type effect = effects

fun tranFunc board (Place (piece, i, j)) = M.replace (i, j) piece board

end
