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

end

signature VECT = sig

  type 'a container
  type index = int
  type size = int

  val index : 'a container * index -> 'a
  val update : 'a container * index * 'a -> 'a container
  val size : 'a container -> size
  val tabulate : size * (index -> 'a) -> 'a container
  val init : size * 'a -> 'a container

end

signature SQUAREMATRIX = sig

  type 'a matrix
  type 'a container = 'a matrix
  type index
  type size = int

  val index : 'a container * index -> 'a
  val update : 'a container * index * 'a -> 'a container
  val size : 'a container -> size
  val init : size * 'a -> 'a container
  val tabulate : size * (index -> 'a) -> 'a container
  val diags : 'a container -> 'a list list

end

signature SQUARE2DMATRIX = sig

  type 'a matrix
  type 'a container = 'a matrix
  type index = (int * int)
  type size = int

  val index : 'a container * index -> 'a
  val update : 'a container * index * 'a -> 'a container
  val size : 'a container -> size
  val init : size * 'a -> 'a container
  val tabulate : size * (index -> 'a) -> 'a container
  val diags : 'a container -> 'a list list

end

functor Square2DMatrixFn (V : VECT) : SQUARE2DMATRIX =
  struct

  type 'a matrix = 'a V.container V.container
  type 'a container = 'a matrix
  type index = (int * int)
  type size = int

  fun index (xs, (i, j)) =
    let val x = V.index (xs, i)
    in V.index (x, j)
    end

  fun update (matrix,(i, j), elem) =
    let val newrow = V.update (V.index (matrix, i), j, elem)
    in V.update (matrix, i, newrow)
    end

  fun size mat = V.size mat

  fun init (i, seed) =
    let fun mkRow _ = V.init (i, seed)
    in V.tabulate (i, mkRow)
    end

  fun tabulate (i, f) =
    let fun mkRow j = V.tabulate (i, fn k => f (j,k))
    in V.tabulate (i, mkRow)
    end

  fun getDiagL mat =
    let val cnt = 0 in
        let fun helper i m = if (i < size m)
                             then index (m, (i, i)) :: helper (i + 1) m
                             else []
        in helper cnt mat
        end
    end

  fun getDiagR mat =
    let val cnt = (0, size mat) in
      let fun helper (i,j) m = if (i < size m)
                           then index (m, (i, j)) :: helper (i+1,j-1) m
                           else []
      in helper cnt mat
      end
    end

  fun diags mat = [getDiagL mat, getDiagR mat]

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

end

structure VectorMatrix = Square2DMatrixFn(VectorVect)
structure ListMatrix = Square2DMatrixFn(ListVect)
structure ArrayMatrix = Square2DMatrixFn(ArrayVect)
