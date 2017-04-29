(* Haskell's ($) operator because why not just fix SMLs syntax? *)
fun (f : ('a -> 'b)) $ (x : 'a) = f x
infix 3 $

(* curry and uncurry because sml seems to prefer tuples for some reason *)
val curry = fn f => fn x => fn y => f (x, y)
val uncurry = fn f => fn (x, y) => f x y
