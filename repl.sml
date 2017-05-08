signature SHOW =
sig
    type a
    val show : a -> string
end

signature IO =
sig
    structure S : SHOW

    val printIO : S.a -> unit
    val read : 'a -> string option
    val say : string -> unit
end

functor Io (structure Sh : SHOW) : IO =
struct

  structure S = Sh

  (* append a new line to a str, this is expensive *)
  fun appendNewLine str = implode $ (explode str) @ [#"\n"]

  fun printIO x = print o appendNewLine o S.show $ x

  (* function to get user input, it doesn't do anything with its argument *)
  fun read _ = TextIO.inputLine TextIO.stdIn

  fun say str = print o appendNewLine $ str

end

signature EVAL =
sig
    (* stucture for state *)
    structure S : STATE

    (* type for an expression to be eval'd *)
    type expr

    (* function that evaluations the expression on some state *)
    val eval : expr -> S.state -> S.state

end

functor Eval (Sh : STATE) : EVAL =
struct
  structure S = Sh

  type expr = S.effect

  fun eval x = S.tranFunc x
end

signature PARSE =
sig
    (* this will only work for games, probably want a more general parser *)
    structure A : ACTION

    val parse : string -> A.action option

end

signature VALIDATE =
sig
    structure S : STATE
    structure A : ACTION

    val validate : A.action -> S.state -> A.action option

    (* message to print if expression is not valid *)
    val notValidMessage : string

end
