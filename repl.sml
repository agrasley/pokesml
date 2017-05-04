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

  fun read x = TextIO.inputLine TextIO.stdIn

end
