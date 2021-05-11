package akka

import akka.BinaryTreeSet.Operation
import akka.BinaryTreeSet.OperationReply.{ContainsResult, OperationFinished}
import akka.actor.{Actor, ActorRef, Props}

object BinaryTreeNode {
  private sealed trait Position

  private case object Left extends Position
  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {

  import BinaryTreeNode._
  import BinaryTreeSet.Operation._

  private var subtrees = Map[Position, ActorRef]()
  private var removed = initiallyRemoved

  override def receive: Receive = {
    case m: Insert => doInsert(m)
    case m: Contains => doContains(m)
    case m: Remove => doRemove(m)
  }

  private def doInsert(m: Insert): Unit = {
    position(m).fold {
      m.requester ! OperationFinished(m.id)
    }(forwardInsert(_, m))
  }

  private def doContains(m: Contains): Unit = {
    position(m).fold {
      m.requester ! ContainsResult(m.id, !removed)
    }(forwardContains(_, m))
  }

  private def doRemove(m: Remove): Unit = {
    position(m).fold {
      removed = true
      m.requester ! OperationFinished(m.id)
    }(forwardRemove(_, m))
  }

  private def position(m: Operation): Option[Position] = {
    if (m.elem < elem) Some(Left)
    else if (m.elem > elem) Some(Right)
    else None
  }

  private def forwardRemove(pos: Position, m: Remove): Unit =
    subtrees.get(pos).fold(m.requester ! OperationFinished(m.id))(_.forward(m))

  private def forwardContains(pos: Position, m: Contains): Unit =
    subtrees.get(pos).fold(m.requester ! ContainsResult(m.id, result = false))(_.forward(m))

  private def forwardInsert(pos: Position, m: Insert): Unit =
    subtrees.get(pos).fold {
      subtrees += (pos -> context.actorOf(props(m.elem, initiallyRemoved = false)))
      m.requester ! OperationFinished(m.id)
    }(_.forward(m))
}
