package knn

sealed abstract class Command

object Command {

  case object Predict extends Command
  case object Verify extends Command

}
