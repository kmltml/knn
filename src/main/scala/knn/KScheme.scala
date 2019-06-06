package knn

sealed abstract class KScheme

object KScheme {

  final case class Value(value: Int) extends KScheme
  case object Everything extends KScheme
  case object Sqrt extends KScheme

  implicit val read: scopt.Read[KScheme] =
    scopt.Read.reads {
      case "all" => Everything
      case "sqrt" => Sqrt
      case k => Value(k.toInt)
    }

}
