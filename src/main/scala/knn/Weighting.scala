package knn

sealed abstract class Weighting

object Weighting {

  case object Const extends Weighting
  case object Inv extends Weighting
  case object Log extends Weighting

  implicit val read: scopt.Read[Weighting] =
    scopt.Read.reads {
      case "const" => Const
      case "inv" => Inv
      case "log" => Log
      case n =>
        throw new IllegalArgumentException(s"$n is not a valid weighting")
    }

}
