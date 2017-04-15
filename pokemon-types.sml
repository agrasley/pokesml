signature POKETYPE = sig

  (* the abstract type of pokemon and moves;
     instantiated by different datatypes for different generations *)
  type pokeType

  (* takes an attacking type (of the the move used) and a defending type (of the
     defending pokemon) and returns the effectiveness value for the types *)
  val effective : pokeType -> pokeType -> real

end


structure GenITypes : POKETYPE =
struct

  (* Gen I didn't have dark, fairy, or steel *)
  datatype genIType
    = Normal
    | Fighting
    | Flying
    | Poison
    | Ground
    | Rock
    | Bug
    | Ghost
    | Fire
    | Water
    | Grass
    | Electric
    | Psychic
    | Ice
    | Dragon

  type pokeType = genIType

  (* TODO *)
  fun effective x y = 1.0

end


structure GenIITypes : POKETYPE =
struct

  (* Gen II added steel and dark *)
  datatype genIIType
    = Normal
    | Fighting
    | Flying
    | Poison
    | Ground
    | Rock
    | Bug
    | Ghost
    | Steel
    | Fire
    | Water
    | Grass
    | Electric
    | Psychic
    | Ice
    | Dragon
    | Dark

  type pokeType = genIIType

  (* TODO *)
  fun effective x y = 1.0

end


structure GenVITypes : POKETYPE =
struct

  (* Gen VI added fairy *)
  datatype genVIType
    = Normal
    | Fighting
    | Flying
    | Poison
    | Ground
    | Rock
    | Bug
    | Ghost
    | Steel
    | Fire
    | Water
    | Grass
    | Electric
    | Psychic
    | Ice
    | Dragon
    | Dark
    | Fairy

  type pokeType = genVIType

  (* TODO *)
  fun effective x y = 1.0

end
