use "game.sml";
use "utils.sml";

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

    (* Constructors *)
    val fromList : 'a list list -> 'a matrix
    val init : size -> 'a -> 'a matrix

    (* Destructors *)
    val toList : 'a matrix -> 'a list

    (* Getters *)
    val lookup : index * index -> 'a matrix -> 'a
    val getRow : index -> 'a matrix -> 'a row
    val getCol : index -> 'a matrix -> 'a col
    val getDiagL : 'a matrix -> 'a vector

    (* Setters and Manipulation*)
    val replace : index * index -> 'a -> 'a matrix -> 'a matrix
    val reverse : 'a matrix -> 'a matrix

    (* Traversal *)
    val map : ('a -> 'b) -> 'a matrix -> 'b matrix

    (* Misc *)
    val isEmpty : 'a matrix -> bool
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

  (* Constructors  *)
  fun fromList xs = Vector.map Vector.fromList $ Vector.fromList xs

  fun init (i, j) seed =
    let fun mkRow _ = List.tabulate (j, fn _ => seed)
    in fromList (List.tabulate (i, mkRow))
    end

  (* Traversal *)
  fun map f mat = Vector.map (Vector.map f) mat

  (* Misc *)
  (* val isEmpty : 'a matrix -> bool *)
  fun isEmpty m = Vector.all (fn x => length x = 0) m

  (* Destructors  *)
  fun toList mat = foldr (fn (x, y) => foldr (op ::) y x) [] mat

  (* Setters *)
  fun replace (i, j) elem matrix =
    let val newrow = Vector.update (Vector.sub (matrix, i), j, elem)
    in Vector.update (matrix, i, newrow)
    end

  fun reverse (mat : 'a Matrix.matrix) : 'a Matrix.matrix
    = Vector.map (Vector.foldl op:: []) mat

  (* Getters *)
  fun lookup (i, j) xs =
    let val x = Vector.sub (xs, i)
    in Vector.sub (x, j)
    end

  fun getRow i mat = Vector.sub (mat, i)

  (* val getCol : index -> 'a matrix -> 'a list option *)
  fun getCol i m = Vector.map (fn x => Vector.sub (x, i)) m
                                        
  (* val getDiagL : 'a matrix -> 'a vector *)
  fun getDiagL mat =
    let val cnt = 0 in
        let fun helper i m = (case (isEmpty m) of
                                  true => []
                                | false => lookup (i, i) m :: helper (i + 1) m)
        in Vector.fromList $ helper cnt mat
        end
    end

  fun getDiagR mat = getDiagL $ reverse mat




end

(**************************** TicTacToe State **********************************)
structure tttState :> STATE =
struct

  datatype cell
    = Empty
    | X
    | O
  
  structure M = Matrix
  
  type state = cell M.matrix
  
  datatype effects
    = Place of cell * M.index * M.index
  
  type effect = effects
  
  fun tranFunc board (Place (piece, i, j)) = M.replace (i, j) piece board

end

(************************* TicTacToe Action ************************************)
(* structure tttAction :> ACTION = *)
(* struct *)

(*   structure S = tttState *)

(*   (* An action is the same as an effect for tic tac toe *) *)
(*   type action = S.effects *)

(*                     fun posAction st =  *)
(* end *)
(************************* TicTacToe Agent  ************************************)
