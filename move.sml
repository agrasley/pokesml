signature MOVE = sig

  structure PokeType : POKETYPE

  (* possibly add status affecting moves in the future
     for now, we're only doing attack moves *)
  datatype category = Special | Physical (* | Status *)

  type move = {
    (* type of the move *)
    mvType : PokeType.pokeType,
    (* number of uses for the move *)
    pp : int,
    (* accuracy as a value from 0-1, representing chance to hit;
       if accuracy is not listed, the attack always hits *)
    accuracy : real option,
    (* integer value of the power of the move;
       if we add non-attack moves, this needs to be an option type *)
    power : int,
    (* category of the move *)
    category : category
  }

end
