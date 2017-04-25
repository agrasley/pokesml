(* Signatures  *)
signature STATE = sig
    (* module that denotes a State, defines what the state is, effects possible
    on the state and a transition function. *)

    (* datatype that defines the state *)
    type state

    (* datatype that defines effects possible on the state *)
    type effect

    (* The transition function, given a state, and an effect to apply, produces
    a new state that is the result of applying the effect to the former state *)
    val tranFunc : state -> effect -> state
end

signature ACTION = sig
    (* module that defines an action *)
    structure State : STATE

    (* datatype that defines what an action is *)
    type action

    (* Function takes a state, and generates all possible actions for that state *)
    val posAction : State.state -> action list

    (* Function takes an action, and a state and generates a list of effects on
    that state *)
    val applyAction : action -> State.state -> State.effect list
end

signature AGENT = sig
    (* module that defines an agent *)
    structure Action : ACTION

    (* An agent should be a higher ordered function that selects an action in
    some way *)
    (* can we do type synonyms for functions? *)
    (* type Agent : Action list -> Action *)

    (* defines the behavior of an agent, given a list of actions, the agent
    function will select an action to take *)
    (*val agentFun : (Action.action list -> Action.State.state -> Action.action) -> Action.action list -> Action.State.state -> Action.action*)
    val agentFun : Action.action list -> Action.State.state -> Action.action
end

(* Functors *)
functor Exec (A:AGENT) =
  struct
    fun step (s:A.Action.State.state) = let
        val actions = A.Action.posAction s
        val selection = A.agentFun actions s
        val effects = A.Action.applyAction selection s
      in
        (*TODO*)
  end

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

(* Notes: We have enough to flesh out tic tac toe, I think if we wrote these out
it would become apparent that we actually have two functors here, one especially
for STATE *)
