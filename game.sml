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
    val tranFunc : effect * state -> state

    val showState : state -> string

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

signature EXEC = sig

  structure Agent : AGENT

  val run : Agent.Action.State.state -> Agent.Action.State.state

end

functor ExecFn (A:AGENT) : EXEC =
  struct

  structure Agent = A

  fun step (f : A.agentFun) st =
    let
      val action = f st
    in
      if A.Action.validAction (action, st) then
        let
          val effects = A.Action.applyAction (action, st)
        in
          foldl A.Action.State.tranFunc st effects
        end
      else (print "Invalid action.\n"; step f st)
    end

  fun runStep i st =
    (print ("Current State:\n" ^ A.Action.State.showState st ^ "\n");
     let
       val newSt = step (List.nth (A.agents, i mod (List.length A.agents))) st
     in
       runStep (i+1) newSt
     end)

  fun run st = runStep 0 st

  end
