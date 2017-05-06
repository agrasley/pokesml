(* Haskell's ($) operator because why not just fix SMLs syntax? *)
fun (f : ('a -> 'b)) $ (x : 'a) = f x
infix 3 $

(* curry and uncurry because sml seems to prefer tuples for some reason *)
val curry = fn f => fn x => fn y => f (x, y)
val uncurry = fn f => fn (x, y) => f x y

(* flip curried arguments *)
val flip = fn f => fn x => fn y => f y x

(* take a default value, a function and a maybe, if nothing return default, if
something apply function *)
fun maybe (default : 'b) (f : 'a -> 'b) (SOME exp) = f exp
  | maybe  default        f              NONE      = default

(* tuple helpers *)
fun fst (x, y) = x
fun snd (x, y) = y
                     
