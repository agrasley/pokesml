(* Haskell's ($) operator because why not just fix SMLs syntax? *)
fun (f : ('a -> 'b)) $ (x : 'a) = f x
infix 3 $
