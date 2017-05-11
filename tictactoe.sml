use "utils.sml";
use "matrix.sml";
use "game.sml";


(* state of a tic tac toe game, extends STATE *)
signature TTTSTATE = sig

  structure Matrix : SQUAREMATRIX

  (* cells in our matrix *)
  datatype cell
    = Empty
    | X
    | O

  include STATE
    where type state = cell * (cell Matrix.container)
    where type effect = Matrix.index
    where type initParams = Matrix.size

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

  type initParams = Matrix.size

  (* switch turns and place a cell *)
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

  (* make an empty board given a boardsize i *)
  fun init i = (X,Matrix.init (i,Empty))

  (* is the board full? *)
  fun isTerminal (_,mat) =
    let
      val hasEmpty = Matrix.foldr (fn (x,y) => (isEmpty x) orelse y) false mat
    in if hasEmpty then
      NONE
    else
      SOME "Game over, man! Game over!"
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

  (* actions are ints that get turned into indices *)
  type action = int

  (* take an int and turn it into a singleton list of the corresponding matrix index *)
  fun applyAction (i, (_,mat)) = [State.Matrix.intToIndex (mat, i)]

  (* is the cell empty you're trying to fill? *)
  fun validAction (i, (_,mat)) =
    State.isEmpty (State.Matrix.index (mat, State.Matrix.intToIndex (mat, i)))

  end

structure TttAction = TttActionFn(TttState)
structure Ttt3DAction = TttActionFn(Ttt3DState)

(* makes random agents that just choose their actions at random *)
functor TttRandomAgentFn (AC : TTTACTION) : AGENT =
  struct

  structure Action = AC

  type agentFun = Action.State.state -> Action.action

  val r = Random.rand (Int.fromLarge (Time.toSeconds(Time.now()) mod 1000),Int.fromLarge (Time.toSeconds(Time.now()) mod 10000))
  val nextInt = Random.randRange (1,1000)

  (* pick a random integer from 1 to 1000 regardless of state *)
  fun randomAgent _ = nextInt r

  val agents = [randomAgent, randomAgent]

  end

structure TttRandomAgent = TttRandomAgentFn(TttAction)
structure TttRandom3DAgent = TttRandomAgentFn(Ttt3DAction)

(* agents representing human players that interact via stdIn *)
functor TttHumanAgentFn (AC : TTTACTION) : AGENT =
  struct

  structure Action = AC

  type agentFun = Action.State.state -> Action.action

  (* keep looping until you get an int from stdIn *)
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

structure TttExecRandom = ExecFn(TttRandomAgent)
structure TttExec3DRandom = ExecFn(TttRandom3DAgent)
