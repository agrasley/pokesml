signature STATE = sig
    (* module that denotes a State, defines what the state is, effects possible
    on the state and a transition function. *)

    (* datatype that defines the state *)
    type state

    (* datatype that defines effects possible on the state *)
    type effect

    (* type of the parameters necessary to initialize the state *)
    type initParams

    (* The transition function, given a state, and an effect to apply, produces
    a new state that is the result of applying the effect to the former state *)
    val tranFunc : effect * state -> state

    (* turn the state into a string for printing *)
    val showState : state -> string

    (* initialize the state given some parameters *)
    val init : initParams -> state

    (* have we reached a terminal state, if so, return SOME message *)
    val isTerminal : state -> string option

end

signature ACTION = sig
    (* module that defines an action *)
    structure State : STATE

    (* datatype that defines what an action is *)
    type action

    (* Function takes an action, and a state and generates a list of effects on
    that state *)
    val applyAction : action * State.state -> State.effect list

    (* is this a valid action to take on the current state? *)
    val validAction : action * State.state -> bool
end

signature AGENT = sig
    (* module that defines an agent *)
    structure Action : ACTION

    (* agents are functions that take a staete and choose an action to take *)
    type agentFun = Action.State.state -> Action.action

    (* value representing all the agents (players) in a game *)
    val agents : agentFun list
end

signature EXEC = sig
  (* runs the game *)

  structure Agent : AGENT

  (* given intialization parameters, run the game *)
  val run : Agent.Action.State.initParams -> unit

end

(* given an agent module, produce a module that can run the game *)
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
       case A.Action.State.isTerminal newSt of
         NONE => runStep (i+1) newSt
         | SOME msg => print
           ("Final State:\n" ^ A.Action.State.showState newSt ^ "\n" ^ msg ^ "\n")
     end)

  fun run i = runStep 0 (A.Action.State.init i)

  end
