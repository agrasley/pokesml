signature GAME = sig
    (* We may need more types here, not sure, perhaps a player type? *)

    (* Data type that represents game state *)
    type gameState
    (* Data Type to represent an action in the game *)
    type action

    (* Transition function for gameState *)
     val turn : action -> gameState -> gameState
     val isTerminal : gameState -> Bool
end

(* I think we might need a functor to actually make the game state. The Ideal
would be to pass in a POKEMON and a MOVE to write the struct *)
