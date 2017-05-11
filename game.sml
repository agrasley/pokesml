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
    val tranFunc : effect -> state -> state

end

signature ACTION = sig
    (* module that defines an action *)
    structure State : STATE

    (* datatype that defines what an action is *)
    type action

    (* Function takes an action, and a state and generates a list of effects on
    that state *)
    val applyAction : action * State.state -> State.effect list

    val validAction : action * State.state -> bool
end

signature AGENT = sig
    (* module that defines an agent *)
    structure Action : ACTION

    type agentFun = Action.State.state -> Action.action

    val agents : agentFun list
end

(*
(* Functors *)
(* This will return a struct, we still need a signature *)
functor Exec (A:AGENT) =
struct
    (* fun step (s:A.Action.State.state) = let *)
    (*     val actions = A.Action.posAction s *)
    (*     val selection = A.agentFun actions s *)
    (*     val effects = A.Action.applyAction selection s *)
    (*   in *)
          (* Top level threading order *)
          (* posAction -> agentFun -> applyAction -> tranFunc -> Loop *)
  end
*)
