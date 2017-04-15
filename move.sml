signature MOVE = sig

  structure PokeType : POKETYPE

  (* abstract datatype of moves *)
  type move
  (* abstract datatype of move categories *)
  type category
  (* abstract datatype of move power *)
  type power

  val getName : move -> string
  val getType : move -> PokeType.pokeType
  val getPp : move -> int
  val getMaxPp : move -> int
  val getAccuracy : move -> real option
  val getPower : move -> power
  val getCategory : move -> category

end

(* TODO *)
structure Move : MOVE =
struct

  (* possibly add status affecting moves in the future
     for now, we're only doing attack moves *)
  datatype cat = Special | Physical (* | Status *)

  type category = cat

  type power = int

  type move = {
    name : string,
    (* type of the move *)
    mvType : PokeType.pokeType,
    (* number of uses for the move *)
    pp : int,
    maxPp : int,
    (* accuracy as a value from 0-1, representing chance to hit;
       if accuracy is not listed, the attack always hits *)
    accuracy : real option,
    (* integer value of the power of the move;
       if we add non-attack moves, this needs to be an option type *)
    power : power,
    (* category of the move *)
    category : category
  }

end
