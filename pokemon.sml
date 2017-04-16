signature POKEMON = sig

    (* Basic data types to describe a pokemon *)
    type pokemon
    type moves
    type pokeType
    type stats

    (* Getters *)
    val getMoves     : pokemon -> moves
    val getType      : pokemon -> pokeType
    val getStats     : pokemon -> stats
    val getLevel     : pokemon -> int
    val getAttack    : pokemon -> int
    val getSpAttack  : pokemon -> int
    val getDefense   : pokemon -> int
    val getSpDefense : pokemon -> int
    val getSpeed     : pokemon -> int
    val getHp        : pokemon -> int
    val getMaxHp     : pokemon -> int
    val getEvasion   : pokemon -> real
    val getAccuracy  : pokemon -> real

    (* Setters *)
    val setMoves     : pokemon -> moves -> pokemon
    val setType      : pokemon -> pokeType -> pokemon
    val setStats     : pokemon -> stats -> pokemon
    val setLevel     : pokemon -> int -> pokemon
    val setAttack    : pokemon -> int -> pokemon
    val setSpAttack  : pokemon -> int -> pokemon
    val setDefense   : pokemon -> int -> pokemon
    val setSpDefense : pokemon -> int -> pokemon
    val setSpeed     : pokemon -> int -> pokemon
    val setHp        : pokemon -> int -> pokemon
    val setMaxHp     : pokemon -> int -> pokemon
    val setEvasion   : pokemon -> real -> pokemon
    val setAccuracy  : pokemon -> real -> pokemon
end

(* functor genPokemon (Args : sig *)
(*                      val p : STRING *)
(*                      structure M : MOVES *)
(*                      structure T : POKETYPE *)
(*                      structure S : STATS *)
(*                  end) : POKEMON = *)
(*   struct *)
(*     type pokemon = p *)

(*     (* Getters *) *)
(*     fun getMoves  *)
