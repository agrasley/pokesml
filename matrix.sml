use "utils.sml";

signature CONTAINER = sig

  type 'a container
  type index
  type size

  val index : 'a container * index -> 'a
  val update : 'a container * index * 'a -> 'a container
  val size : 'a container -> size
  val tabulate : size * (index -> 'a) -> 'a container
  val init : size * 'a -> 'a container
  val foldl  : ('a * 'b -> 'b) -> 'b -> 'a container -> 'b
  val foldr  : ('a * 'b -> 'b) -> 'b -> 'a container -> 'b

end

signature SQUAREMATRIX = sig

  include CONTAINER where type size = int

  val intToIndex : 'a container * int -> index

end

signature VECT = sig

  include SQUAREMATRIX where type index = int

end

signature SQUARE2DMATRIX = sig

  include SQUAREMATRIX where type index = int * int

end

functor SquareMatrixFn (structure V : VECT
                        structure M : SQUAREMATRIX) : SQUAREMATRIX =
  struct

  type 'a container = 'a V.container M.container
  type index = M.index * V.index
  type size = int

  fun index (xs, (i, j)) =
    let val x = M.index (xs, i)
    in V.index (x, j)
    end

  fun update (matrix,(i, j), elem) =
    let val newrow = V.update (M.index (matrix, i), j, elem)
    in M.update (matrix, i, newrow)
    end

  fun size mat = M.size mat

  fun init (i, seed) =
    let fun mkRow _ = V.init (i, seed)
    in M.tabulate (i, mkRow)
    end

  fun tabulate (i, f) =
    let fun mkRow j = V.tabulate (i, fn k => f (j,k))
    in M.tabulate (i, mkRow)
    end

  fun foldr f acc mat =
    M.foldr (fn (x, xs) => V.foldr f xs x) acc mat

  fun foldl f acc mat =
    M.foldl (fn (x, xs) => V.foldl f xs x) acc mat

  fun intToIndex (mat, i) =
    let
      val s = M.size mat
      val j = M.intToIndex (mat, i div s)
      val k = i mod s
    in
      (j,k)
    end

  end

structure VectorVect : VECT =
struct

  open Vector

  type 'a container = 'a vector
  type index = int
  type size = int

  val index = sub
  val size = length
  fun init (i, seed) = tabulate (i, fn _ => seed)
  fun intToIndex (xs, i) = i mod (size xs)

end

structure ListVect : VECT =
struct

  open List

  type 'a container = 'a list
  type index = int
  type size = int

  val index = nth

  fun update (x::xs,0,a) = a::xs
    | update (x::xs,n,a) = x::update (xs,n-1,a)
    | update _           = raise Subscript

  val size = length

  fun init (i, seed) = tabulate (i, fn _ => seed)

  fun intToIndex (xs, i) = i mod (size xs)

end

structure ArrayVect : VECT =
struct

  type 'a container = 'a array
  type index = int
  type size = int

  val index = Array.sub
  fun update (a,n,elem) = (Array.update (a,n,elem); a);
  val size = Array.length
  val tabulate = Array.tabulate
  val init = Array.array
  val foldl = Array.foldl
  val foldr = Array.foldr

  fun intToIndex (xs, i) = i mod (size xs)

end

structure VectorMatrix = SquareMatrixFn(
    structure V = VectorVect
    structure M = VectorVect)

structure ListMatrix = SquareMatrixFn(
    structure V = ListVect
    structure M = ListVect)

structure ArrayMatrix = SquareMatrixFn(
    structure V = ArrayVect
    structure M = ArrayVect)

structure Vector3DMatrix = SquareMatrixFn(
    structure V = VectorVect
    structure M = VectorMatrix)

structure List3DMatrix = SquareMatrixFn(
    structure V = ListVect
    structure M = ListMatrix)

structure Array3DMatrix = SquareMatrixFn(
    structure V = ArrayVect
    structure M = ArrayMatrix)
