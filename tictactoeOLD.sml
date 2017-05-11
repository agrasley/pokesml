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

    (* doing it with type synonyms led to a bunch of typy synonym errors between structures like this *)
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
    val toList : 'a matrix -> 'a list list

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

  (**************************** Destructors ************************************)
  fun toList mat = Vector.toList $ Vector.map (Vector.toList) mat

  (**************************** Misc *******************************************)
  fun isEmpty m = (fn x => 0 = x) o List.length o List.concat o toList $ m

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
        let fun helper i m = if (i < rowLength m) 
                             then lookup (i, i) m :: helper (i + 1) m
                             else []
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
    | X 
    | O 

  type state = cell matrix

  datatype effect
    = Place of cell * int * int

  fun tranFunc (Place (Empty, x, y)) board  = board
    | tranFunc (Place (X    , x, y)) board = replace (x, y) X board
    | tranFunc (Place (O    , x, y)) board = replace (x, y) O board

  fun isEmpty Empty = true
    | isEmpty _     = false

end

(************************* TicTacToe Action ************************************)
(* when I transparently ascribe we lose access to A.state.Place for some reason *)
(* structure tttAction : ACTION = struct *)
structure tttAction = struct

  (* tttState is still of type : STATE, but is enriched *)
  structure S = tttState

  (* An action is the same as an effect for tic tac toe *)
  type action = S.effect

  (* this will be slow, optimize later by implementing zip in terms of foldr *)
  fun enumerateList xs =
    let fun helper cnt (x::xs) = (cnt, x) :: helper (cnt + 1) xs
          | helper cnt []      = []
    in helper 0 xs
    end

  fun enumNestedList xs = enumerateList o map enumerateList $ xs

  (* This function is specfic to repair the list structure coming from the
     enumerate functions *)
  fun repair xs = map (fn (i, ys) => map (fn (j, elem) => (elem, i, j)) ys) xs

  fun posAction (st : S.state) =
    List.map S.Place o List.filter (S.isEmpty o fst3) o
    List.concat o repair o enumNestedList o S.toList $ st

  fun applyAction (a, st) = ([a], st)

end
(************************* TicTacToe Agent  ************************************)
functor Agent (A : ACTION) : AGENT =
struct

  structure Action = A

  type agentFun = Action.action list -> Action.S.state
                  -> Action.action * Action.S.state

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

  fun show S.Empty = "_"
    | show S.X     = "X"
    | show S.O     = "O"

end 

structure tttShow : SHOW = struct

  structure S = tttState
  structure CS = cellShow

  type a = S.state

  (* This will need to be changed for pretty printing *)
  fun toStrList mat = S.toList o S.mapElem CS.show $ mat

  fun show mat =
    String.concat o List.concat o map (fn str => str @ ["\n"]) $ toStrList mat

end

structure tttIO = Io(structure Sh = tttShow)

(************************* TicTacToe Eval **************************************)
structure tttEval = Eval(tttState)

(************************* TicTacToe Parse *************************************)
structure tttParse : PARSE = struct
  structure A = tttAction

  fun parseDigit x = Int.fromString x

  (* this is really just a blind tokenize *)
  fun parseHelper (x::xs)
    = (case parseDigit x of
           NONE => parseHelper xs
         | SOME i  => i :: parseHelper xs)
    | parseHelper nil     = nil

  fun shitParse (str) =
    (case explode str
      of nil => NONE
       | (x::xs) =>
         (case Char.toString x
                             (* a code smell indeed *)
           of "X" => (case parseHelper $ map Char.toString xs
                       of (x::y::xs) => SOME o A.S.Place $ (A.S.X, x, y)
                       |  _          => NONE)
            | "O" => (case parseHelper $ map Char.toString xs
                       of (x::y::xs) => SOME o A.S.Place $ (A.S.O, x, y)
                       |  _          => NONE)
            | _  => NONE))

  (* A useful reminder to how much the parse could improve *)
  fun parse str = shitParse str

end

structure tttValidate : VALIDATE = struct

  structure A = tttAction
  structure S = tttState

  (* Takes it off!!! It hurtses us!! *)
  fun validate (A.S.Place (A.S.X, x, y)) mat =
    if (x <= (fst $ A.S.dimensions mat)) andalso (y <= (snd $ A.S.dimensions mat))
       andalso (A.S.Empty = A.S.lookup (x, y) mat)
    then SOME o A.S.Place $ (S.X, x, y)
    else NONE
    | validate (A.S.Place (A.S.O, x, y)) mat =
      if (x <= (fst $ A.S.dimensions mat)) andalso (y <= (snd $ A.S.dimensions mat))
         andalso (A.S.Empty = A.S.lookup (x, y) mat)
      then SOME o A.S.Place $ (A.S.O, x, y)
      else NONE
    | validate (A.S.Place (A.S.Empty, x, y)) mat =
      if (x <= (fst $ A.S.dimensions mat)) andalso (y <= (snd $ A.S.dimensions mat))
         andalso (A.S.Empty = A.S.lookup (x, y) mat)
      then SOME o A.S.Place $ (A.S.Empty, x, y)
      else NONE


  val notValidMessage = "Bad Move!"
end

(************************* TicTacToe Main **************************************)
structure Main = struct

   (* structs  *)
  structure A = tttAction
  structure P = tttParse
  structure E = tttEval
  structure V = tttValidate
  structure I = tttIO

  val board = A.S.init (3, 3) A.S.Empty

  (* 1. Print board 2. input action 3. Execute Action 4. return state *)
  (* Write out the main then abstraction into a signature *)
  fun inputAndValidate board =
    case I.read 0
     of NONE   => NONE
      | SOME i => maybe NONE ((flip V.validate) board) (P.parse i)

  (* fun execAction action = E.eval $ actionToEffect action *)
  fun execAction action = E.eval action

  fun listCompare [] []           = true
    | listCompare (x::xs) (y::ys) = (x = y) andalso listCompare xs ys
    | listCompare _       _       = false

  val xWins = List.tabulate (3, fn _ => A.S.X)
  val oWins = List.tabulate (3, fn _ => A.S.O)

  fun winConditions board = List.map (fn f => Vector.toList $ f board)
                                     [ A.S.getRow 0, A.S.getRow 1, A.S.getRow 2
                                     , A.S.getCol 0, A.S.getCol 1, A.S.getCol 2
                                     , A.S.getDiagL, A.S.getDiagR
                                     ]

  fun isTerminal board =
    let val result = winConditions board
    in foldl (uncurry or) false $ map (listCompare xWins) result orelse
       foldl (uncurry or) false $ map (listCompare oWins) result
    end

  fun driver board =
    (case I.say "Board: " >> I.printIO board >> isTerminal board
      of (* TODO figure out winner *)
         true => I.say "Game Over!"
       | false => (case I.say "Please make a move: " >> inputAndValidate board
                    of NONE     =>  I.say V.notValidMessage >> driver board 
                     | SOME move => let val newBoard = execAction move board
                                    in driver newBoard
                                    end))

end
