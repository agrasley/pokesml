(* Haskell's ($) operator because why not just fix SMLs syntax? *)
infix 3 $
fun (f : ('a -> 'b)) $ (x : 'a) = f x

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

fun fst3  (x, y, z) = x
fun snd3  (x, y, z) = y
fun thrd3 (x, y, z) = z

fun or false false = false
  | or _     _     = true

infix 1 >>
fun (a : 'a) >> (x : 'b)  = (a; x)
