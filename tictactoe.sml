use "game.sml";
use "utils.sml";
use "repl.sml";

(************************ Sample tic tac toe environment ***********************)
signature MATRIX =
sig
    type ''a row = ''a vector
    type ''a col = ''a vector

    type 'a matrix = 'a row col
    exception OutOfBounds

    (* doing it this way led to a bunch of typy synonym errors like this *)
    (*/tmp/emacs-region726BiT:233.8-235.58 Error: case object and rules don't agree [tycon mismatch] *)
    (* rule domain: index option *)
    (* object: int option *)
    (* in expression: *)
    (*    (case (parseDigit x) *)
    (*      of NONE => parseHelper xs *)
    (*       | SOME (i : index) => i :: parseHelper xs) *)
    (* type index *)
    (* type size = index * index *)
    (* index IS JUST A TYPE SYNONYM for Int *)

    type size = int * int
    (* Constructors *)
    val fromList : 'a list list -> 'a matrix
    val init : size -> 'a -> 'a matrix

    (* Destructors *)
    val toList : 'a matrix -> 'a list

    (* Getters *)
    val lookup : int * int -> 'a matrix -> 'a
    val getRow : int -> 'a matrix -> 'a row
    val getCol : int -> 'a matrix -> 'a col
    val getDiagL : 'a matrix -> 'a vector
    val getDiagR : 'a matrix -> 'a vector
    val rowLength : 'a matrix -> int
    val colLength : 'a matrix -> int
    val dimensions : 'a matrix -> size

    (* Setters and Manipulation*)
    val replace : int * int -> 'a -> 'a matrix -> 'a matrix
    val reverse : 'a matrix -> 'a matrix

    (* Traversal *)
    val mapElem : ('a -> 'b) -> 'a matrix -> 'b matrix
    val mapRow : ('a row -> 'b) -> 'a matrix -> 'b vector
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
  type size = int * int

  exception OutOfBounds

  open Vector

  (*************************** Constructors ************************************)
  fun fromList xs = Vector.map Vector.fromList $ Vector.fromList xs

  fun init (i, j) seed =
    let fun mkRow _ = List.tabulate (j, fn _ => seed)
    in fromList (List.tabulate (i, mkRow))
    end

  (************************** Traversal ****************************************)
  fun mapElem f mat = Vector.map (Vector.map f) mat
  fun mapRow f (mat : 'a matrix) = Vector.map f mat

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

  fun rowLength mat = Vector.foldr (fn (_, y) => 1 + y) 0 o getRow 0 $ mat

  fun colLength mat = Vector.foldr (fn (_, y) => 1 + y) 0 mat

  fun dimensions mat = (rowLength mat, colLength mat)

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
    = Empty of int * int
    | X of int * int
    | O of int * int

  type state = cell matrix

  datatype effect
    = Place of cell

  fun tranFunc (Place (Empty _)) board  = board
    | tranFunc (Place (X coords)) board = replace coords (X coords) board
    | tranFunc (Place (O coords)) board = replace coords (O coords) board

  fun isEmpty (Empty _) = true
    | isEmpty _         = false

end

(************************* TicTacToe Action ************************************)
(* when I transparently ascribe we lose access to A.state.Place for some reason *)
(* structure tttAction : ACTION = struct *)
structure tttAction = struct

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

structure tttAgents = Agent(tttAction)

(************************* TicTacToe Show **************************************)
structure cellShow : SHOW = struct
  structure S = tttState

  type a = S.cell

  fun show (S.Empty _) = " "
    | show (S.X _)     = "X"
    | show (S.O _)     = "O"

end 

structure tttShow :> SHOW = struct

  structure S = tttState
  structure CS = cellShow

  type a = S.state

  (* This will need to be changed for pretty printing *)
  fun show mat = String.concat o S.toList o S.mapElem CS.show $ mat

end

structure tttIO = Io(structure Sh = tttShow)

(************************* TicTacToe Eval **************************************)
functor Eval (Sh : STATE) :> EVAL = struct
  structure S = Sh

  type expr = S.effect

  fun eval x = S.tranFunc x
end

structure tttEval = Eval(tttState)
(************************* TicTacToe Parse**************************************)
(* functor Parse (A : ACTION) :> PARSE = struct *)
(*   structure A = A *)

(*   (* This is needed for the parse, I don't think it should be in the sig *) *)
(*   open tttState *)

(*   fun parseDigit x = Int.fromString x *)

(*   (* this is really just a blind tokenize *) *)
(*   fun parseHelper (x::xs) : Matrix.index list *)
(*     = (case parseDigit x of *)
(*            NONE => parseHelper xs *)
(*          | SOME (i : Matrix.index) => i :: parseHelper xs) *)
(*     | parseHelper nil     = nil *)

(*   fun shitParse (str) = *)
(*     (case explode str *)
(*       of nil => NONE *)
(*        | (x::xs) => (case Char.toString x *)
(*                               (* a code smell indeed *) *)
(*                       of "X" => (case parseHelper $ map Char.toString xs *)
(*                                   of nil => NONE  *)
(*                                    | (x::y::xs) => SOME o Place o X $ (x, y)) *)
(*                        | "O" => (case parseHelper $ map Char.toString xs *)
(*                                   of nil => NONE *)
(*                                    | (x::y::xs) => SOME o Place o O $ (x, y)))) *)

(*   (* A useful reminder to how much the parse could improve *) *)
(*   fun parse str = shitParse str *)

(* end *)

structure Validate :> VALIDATE = struct

  structure A = tttAction
  structure S = tttState

  (* Takes it off!!! It hurtses us!! *)
  fun validate (A.State.Place (S.X (x, y))) mat =
    if (x <= (fst $ S.dimensions mat)) andalso (y <= (snd $ S.dimensions mat))
    then SOME o A.State.Place o S.X $ (x, y)
    else NONE
    | validate (A.State.Place (S.O (x, y))) mat =
      if (x <= (fst $ S.dimensions mat)) andalso (y <= (snd $ S.dimensions mat))
      then SOME o A.State.Place o S.X $ (x, y)
      else NONE
    | validate (A.State.Place (S.Empty (x, y))) mat =
      if (x <= (fst $ S.dimensions mat)) andalso (y <= (snd $ S.dimensions mat))
      then SOME o A.State.Place o S.X $ (x, y)
      else NONE


  val notValidMessage = "Bad Move!"
end
