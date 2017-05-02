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
    val foldr : ('a -> 'b -> 'b) -> 'b -> 'a matrix -> 'b
    val filter : ('a -> bool) -> 'a matrix -> 'a list

    (* Misc *)
    val isEmpty : 'a matrix -> bool
end

structure Matrix :> MATRIX =
struct

  type ''a row = ''a vector
  type ''a col = ''a vector
  type 'a matrix = 'a row col

  type index = int
  type dimensions = index * index
  type size = index * index

  exception OutOfBounds

  open Vector

  (*************************** Constructors ************************************)
  fun fromList xs = Vector.map Vector.fromList $ Vector.fromList xs

  fun init (i, j) seed =
    let fun mkRow _ = List.tabulate (j, fn _ => seed)
    in fromList (List.tabulate (i, mkRow))
    end

  (************************** Traversal ****************************************)
  fun map f mat = Vector.map (Vector.map f) mat

  fun foldr f acc mat =
    Vector.foldr (fn (x, xs) => Vector.foldr (uncurry f) xs x) acc mat

  fun filter f = foldr (fn x => fn xs => if f x then x::xs else xs) []

  (**************************** Misc *******************************************)
  fun isEmpty m = Vector.all (fn x => length x = 0) m

  (**************************** Destructors ************************************)
  fun toList mat = Vector.foldr (fn (x, y) => Vector.foldr (op ::) y x) [] mat

  (***************************** Setters ***************************************)
  fun replace (i, j) elem matrix =
    let val newrow = Vector.update (Vector.sub (matrix, i), j, elem)
    in Vector.update (matrix, i, newrow)
    end

  fun reverse (mat : 'a matrix) : 'a matrix
    = Vector.map (Vector.fromList o Vector.foldl op:: []) mat

  (***************************** Getters ***************************************)
  fun lookup (i, j) xs =
    let val x = Vector.sub (xs, i)
    in Vector.sub (x, j)
    end

  fun getRow i mat = Vector.sub (mat, i)

  fun getCol i m = Vector.map (fn x => Vector.sub (x, i)) m

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
(* Notice this doesn't use the state signature, if it did we wouldn't be able to *)
(* access the Place constructor *)
structure tttState = struct

  open Matrix
  datatype cell
    = Empty
    | X of index * index
    | O of index * index

  type state = cell matrix

  datatype effect
    = Place of cell

  fun tranFunc board (Place (Empty)) = board
    | tranFunc board (Place (X coords)) = replace coords (X coords) board
    | tranFunc board (Place (O coords)) = replace coords (O coords) board

  fun isEmpty Empty  = true
    | isEmpty _      = false

end

(************************* TicTacToe Action ************************************)
structure tttAction : ACTION = struct

  (* tttState is still of type : STATE, but is enriched *)
  structure State = tttState

  (* An action is the same as an effect for tic tac toe *)
  type action = State.effect

  fun posAction (st : State.state) =
    List.map State.Place $ State.filter State.isEmpty st

  fun applyAction (a, st) = ([a], st)

end
(************************* TicTacToe Agent  ************************************)
functor Agent (A : ACTION) : AGENT =
struct

  structure Action = A

  type agentFun = Action.action list -> Action.State.state
                  -> Action.action * Action.State.state

  (* A function to make agentFunctions given some HOF *)
  fun agentFunGen f xs st = (f xs, st)

  (* some dumb agents *)
  fun agentOne xs = agentFunGen hd xs
  fun agentTwo xs = agentFunGen List.last xs

  val agents = [agentOne, agentOne]
end

structure tttAgents = tttAgent(tttAction)
