import stainless.collection._
import stainless.annotation._

// https://github.com/tlaplus/Examples/blob/master/specifications/transaction_commit/TwoPhase.tla
object TwoPhase {
  case class RM(n: BigInt) {
    require(0 <= n && n <= lastRM)
  }
  
  @extern
  def lastRM : BigInt = { BigInt(10) } // number of resource managers, 0..lastRM

  def allRMs: List[RM] = allFrom(0)
  
  def allFrom(k: BigInt): List[RM] = { // in order
    require(0 <= k)
    if (k <= lastRM) RM(k) :: allFrom(k+1)
    else Nil[RM]()
  }

  sealed abstract class RMState
  case object Working extends RMState
  case object Prepared extends RMState
  case object Committed extends RMState
  case object Aborted extends RMState

  sealed abstract class TMState
  case object TInit extends TMState
  case object TCommitted extends TMState
  case object TAborted extends TMState
 
  abstract class Message
  case class MPrepared(rm: RM) extends Message
  case object MCommit extends Message
  case object MAbort extends Message
  
  case class State(rmState: RM => RMState,
                   tmState: TMState,
                   tmPrepared: List[RM],
                   msgs: List[Message])

  def allWorking(rm: RM): RMState = Working
  
  def TPInit: State = 
    State(allWorking, TInit, List[RM](), List[Message]())

  def insertNoDup[A](m: A, l: List[A]): List[A] = {
    if (l.content.contains(m)) l
    else m :: l
  }
  
  def tMCommit(s: State): State = {
    if ((s.tmState == TInit) && s.tmPrepared == allRMs) {
      s.copy(tmState = TCommitted, msgs = insertNoDup(MCommit, s.msgs))
    } else s
  }

  def tMAbort(s: State): State = {
    if (s.tmState == TInit) {
      s.copy(tmState = TAborted, msgs = insertNoDup(MAbort, s.msgs))
    } else s
  }

  sealed abstract class Action
  case object ACommit extends Action
  case object AAbort extends Action

  type Schedule = List[Action]

  def someoneAborts(s: State) =
    allRMs.exists((rm: RM) => s.rmState(rm) == Aborted)

  def someoneCommits(s: State) =
    allRMs.exists((rm: RM) => s.rmState(rm) == Committed)
  
  def inv(s: State): Boolean = {
    !(someoneAborts(s) && someoneCommits(s))
  }
  
  def run(s: State, schedule: Schedule): State = {
    require(inv(s))
    schedule match {
      case Nil() => s
      case ACommit :: rest => run(tMCommit(s), rest)
      case AAbort :: rest => run(tMAbort(s), rest)
    }
  }
}
