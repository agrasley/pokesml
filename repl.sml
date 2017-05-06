signature SHOW =
sig
    type a
    val show : a -> string
end

signature IO =
sig
    structure S : SHOW

    val print : S.a -> unit
    val read : 'a -> string option
end

functor Io (structure Sh : SHOW) : IO =
struct

  structure S = Sh

  fun print x = let val str = S.show x
               in TextIO.output (TextIO.stdOut, str)
               end

  (* function to get user input, it doesn't do anything with its argument *)
  fun read _ = TextIO.inputLine TextIO.stdIn

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

(* signature CONTROLLER = *)
(* sig *)
(*     (* type that we are parsing to *) *)
(*     type result *)

signature PARSE =
sig
    structure A : ACTION

    val parse : string -> A.action

    (* check if an expression is valid *)
    val isValid : string -> bool

    (* message to print if expression is not valid *)
    val notValidMessage : string
end
