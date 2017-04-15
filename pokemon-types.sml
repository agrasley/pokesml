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
  fun effective Normal Rock = 0.5
    | effective Normal Ghost = 0.0
    | effective Normal Steel = 0.5
    | effective Fight Normal = 2.0
    | effective Fight Flying = 0.5
    | effective Fight Poison = 0.5
    | effective Fight Rock = 2.0
    | effective Fight Bug = 0.5
    | effective Fight Ghost = 0.0
    | effective Fight Steel = 2.0
    | effective Fight Psychic = 0.5
    | effective Fight Ice = 2.0
    | effective Fight Dark = 2.0
    | effective Fight Fairy = 0.5
    | effective Flying Fight = 2.0
    | effective Flying Rock = 0.5
    | effective Flying Bug = 2.0
    | effective Flying Steel = 0.5
    | effective Flying Grass = 2.0
    | effective Flying Electric = 0.5
    | effective Poison Poison = 0.5
    | effective Poison Ground = 0.5
    | effective Poison Rock = 0.5
    | effective Poison Ghost = 0.5
    | effective Poison Steel = 0.0
    | effective Poison Grass = 2.0
    | effective Poison Fairy = 2.0
    | effective Ground Flying = 0.0
    | effective Ground Poison = 2.0
    | effective Ground Rock = 2.0
    | effective Ground Bug = 0.5
    | effective Ground Steel = 2.0
    | effective Ground Fire = 2.0
    | effective Ground Grass = 0.5
    | effective Ground Electric = 2.0
    | effective Rock Fight = 0.5
    | effective Rock Flying = 2.0
    | effective Rock Ground = 0.5
    | effective Rock Bug = 2.0
    | effective Rock Steel = 0.5
    | effective Rock Fire = 2.0
    | effective Rock Ice = 2.0
    | effective Bug Fight = 0.5
    | effective Bug Flying = 0.5
    | effective Bug Poison = 0.5
    | effective Bug Ghost = 0.5
    | effective Bug Steel = 0.5
    | effective Bug Fire = 0.5
    | effective Bug Grass = 2.0
    | effective Bug Psychic = 2.0
    | effective Bug Dark = 2.0
    | effective Bug Fairy = 0.5
    | effective Ghost Normal = 0.0
    | effective Ghost Ghost = 2.0
    | effective Ghost Psychic = 2.0
    | effective Ghost Dark = 0.5
    | effective Steel Rock = 2.0
    | effective Steel Steel = 0.5
    | effective Steel Fire = 0.5
    | effective Steel Water = 0.5
    | effective Steel Electric = 0.5
    | effective Steel Ice = 2.0
    | effective Steel Fairy = 2.0
    | effective Fire Rock = 0.5
    | effective Fire Bug = 2.0
    | effective Fire Steel = 2.0
    | effective Fire Fire = 0.5
    | effective Fire Water = 0.5
    | effective Fire Grass = 2.0
    | effective Fire Ice = 2.0
    | effective Fire Dragon = 0.5
    | effective Water Ground = 2.0
    | effective Water Rock = 2.0
    | effective Water Fire = 2.0
    | effective Water Water = 0.5
    | effective Water Grass = 0.5
    | effective Water Dragon = 0.5
    | effective Grass Flying = 0.5
    | effective Grass Poison = 0.5
    | effective Grass Ground = 2.0
    | effective Grass Rock = 2.0
    | effective Grass Bug = 0.5
    | effective Grass Steel = 0.5
    | effective Grass Fire = 0.5
    | effective Grass Water = 2.0
    | effective Grass Grass = 0.5
    | effective Grass Dragon = 0.5
    | effective Electric Flying = 2.0
    | effective Electric Ground = 0.0
    | effective Electric Water = 2.0
    | effective Electric Grass = 0.5
    | effective Electric Electric = 0.5
    | effective Electric Dragon = 0.5
    | effective Psychic Fight = 2.0
    | effective Psychic Poison = 2.0
    | effective Psychic Steel = 0.5
    | effective Psychic Psychic = 0.5
    | effective Psychic Dark = 0.0
    | effective Ice Flying = 2.0
    | effective Ice Ground = 2.0
    | effective Ice Steel = 0.5
    | effective Ice Fire = 0.5
    | effective Ice Water = 0.5
    | effective Ice Grass = 2.0
    | effective Ice Ice = 0.5
    | effective Ice Dragon = 2.0
    | effective Dragon Steel = 0.5
    | effective Dragon Dragon = 2.0
    | effective Dragon Fairy = 0.0
    | effective Dark Fight = 0.5
    | effective Dark Ghost = 2.0
    | effective Dark Psychic = 2.0
    | effective Dark Dark = 0.5
    | effective Dark Fairy = 0.5
    | effective Fairy Fight = 2.0
    | effective Fairy Poison = 0.5
    | effective Fairy Steel = 0.5
    | effective Fairy Fire = 0.5
    | effective Fairy Dragon = 2.0
    | effective Fairy Dark = 2.0
    | effective x y = 1.0
end
