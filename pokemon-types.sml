signature POKETYPE = sig

  (* the types of pokemon and moves *)
  datatype pokeType
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

  (* takes an attacking type (of the the move used) and a defending type (of the
     defending pokemon) and returns the effectiveness value for the types *)
  val effective : pokeType -> pokeType -> real

end
