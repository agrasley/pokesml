use "utils.sml";
use "matrix.sml";
use "game.sml";

signature TTTSTATE = sig

  structure Matrix : SQUAREMATRIX

  datatype cell
    = Empty
    | X
    | O

  include STATE
    where type state = cell * (cell Matrix.container)
    where type effect = Matrix.index

  val isEmpty : cell -> bool

end

functor TttStateFn (M : SQUAREMATRIX) : TTTSTATE =
  struct

  structure Matrix = M

  datatype cell
    = Empty
    | X
    | O

  type state = cell * (cell Matrix.container)

  type effect = Matrix.index

  fun tranFunc (i, (X,board)) = (O, Matrix.update (board,i,X))
    | tranFunc (i, (O,board)) = (X, Matrix.update (board,i,O))

  fun isEmpty Empty = true
    | isEmpty _     = false

  fun showCell X     _ = "|X|"
    | showCell O     _ = "|O|"
    | showCell Empty i = "|" ^ Int.toString i ^ "|"


  fun showState (_,mat) =
    let
      val s = Matrix.size mat
      val l = implode (List.tabulate (s*3,fn _ => #"-"))
      val f = fn (c, (acc, i)) => if i mod (s*s) = 0 then
          (acc ^ "\n" ^ l ^ "\n" ^ showCell c i, i+1)
        else if i mod s = s-1 then
          (acc ^ showCell c i ^ "\n" ^ l ^ "\n", i+1)
        else (acc ^ showCell c i, i+1)
    in
      fst (Matrix.foldl f ("",0) mat)
    end

  end

structure TttState = TttStateFn(ArrayMatrix)
structure Ttt3DState = TttStateFn(Array3DMatrix)


signature TTTACTION = sig

  include ACTION
    where type action = int

end


functor TttActionFn (ST : TTTSTATE) : TTTACTION =
  struct

  structure State = ST

  type action = int

  fun applyAction (i, (_,mat)) = [State.Matrix.intToIndex (mat, i)]

  fun validAction (i, (_,mat)) =
    State.isEmpty (State.Matrix.index (mat, State.Matrix.intToIndex (mat, i)))

  end

structure TttAction = TttActionFn(TttState)
structure Ttt3DAction = TttActionFn(Ttt3DState)

functor TttRandomAgentFn (AC : TTTACTION) : AGENT =
  struct

  structure Action = AC

  type agentFun = Action.State.state -> Action.action

  val r = Random.rand (Int.fromLarge (Time.toSeconds(Time.now()) mod 1000),Int.fromLarge (Time.toSeconds(Time.now()) mod 10000))
  val nextInt = Random.randRange (1,1000)

  fun randomAgent _ = nextInt r

  val agents = [randomAgent, randomAgent]

  end

structure TttRandomAgent = TttRandomAgentFn(TttAction)
structure TttRandom3DAgent = TttRandomAgentFn(Ttt3DAction)

functor TttHumanAgentFn (AC : TTTACTION) : AGENT =
  struct

  structure Action = AC

  type agentFun = Action.State.state -> Action.action

  fun humanAgent st =
    (print "Please enter the number of the cell you want to fill:\n";
    let
      val msg = "Invalid input. Please try again.\n"
    in case TextIO.inputLine TextIO.stdIn of
      NONE => (print msg; humanAgent st)
      | SOME x => case Int.fromString x of
        NONE => (print msg; humanAgent st)
        | SOME y => y
    end)

  val agents = [humanAgent, humanAgent]

  end

structure TttHumanAgent = TttHumanAgentFn(TttAction)
structure TttHuman3DAgent = TttHumanAgentFn(Ttt3DAction)

structure TttExecHuman = ExecFn(TttHumanAgent)
structure TttExec3DHuman = ExecFn(TttHuman3DAgent)
