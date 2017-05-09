structure PokemonState = struct

  type pokeState = {maxHp: int, hp: int, moves: move list, status: status}

  datatype turn = First | Second

  type state = {whoseTurn: turn, pkmn1: pokeState, pkmn2: pokeState}

  datatype status = None | Sleep

  datatype move = Attack {name: string, power: int, pType: pokeType, acc: real}
                | Status {name: string, effects: effect list}

  datatype target = Opponent | Self

  datatype effect = Damage of target * int
                  | Heal of target * int
                  | Sleep of target
                  | EndTurn

  fun damagePkmn i {maxHp=mhp, hp=ohp, moves=m, status=s} = if ohp - i < 0 then
                                                              {maxHp=mhp, hp=0, moves=m, status=s}
                                                            else {maxHp=mhp, hp=ohp - i, moves=m, status=s}

  fun updateStatus s {maxHp=mhp, hp=h, moves=m, status=_} = {maxHp=mhp, hp=h, moves=m, status=s}

  fun healPkmn i {maxHp=mhp, hp=ohp, moves=m, status=s} = if ohp + i > mhp then
                                                            {maxHp=mhp, hp=mhp, moves=m, status=s}
                                                          else {maxHp=mhp, hp=ohp + i, moves=m, status=s}

  fun tranFunc (Damage Self i) st = case (#whoseTurn st) of
                                      First =>

end

structure PokemonAction = struct

  structure State = PokemonState
