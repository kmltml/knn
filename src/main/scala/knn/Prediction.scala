package knn

object Prediction {

  def predict(
      model: Vector[Vector[Double]],
      knownColums: Vector[Int],
      unknownColumns: Vector[Int],
      input: Vector[Double],
      metric: Metric,
      k: Int,
      weighting: Weighting
  ): Vector[Double] = {
    val neighbours = model
      .map(r => (r, metric(knownColums.map(r(_)), input))) // compute distances using known columns
      .sortBy(_._2) // sort by distances ascending
      .take(k) // take first k results
      .map(r => unknownColumns.map(r._1(_))) // take only unknown columns
    val weights = (1 to neighbours.size).map { d =>
      weighting match {
        case Weighting.Const => 1.0
        case Weighting.Inv => 1.0 / d
        case Weighting.Log => math.log(1.0 + d.toDouble)
      }
    }
    unknownColumns.indices.map { c => // for each unknown column
      (neighbours.map(_(c)) zip weights)
        .map(Function.tupled(_ * _))
        .sum / weights.sum // compute the average
    }.toVector
  }

  def predictAll(
      model: Vector[Vector[Double]],
      knownColums: Vector[Int],
      unknownColumns: Vector[Int],
      input: Vector[Vector[Double]],
      metric: Metric,
      k: KScheme,
      weights: Weighting
  ): Vector[Vector[Double]] = {
    val kv = k match {
      case KScheme.Everything => model.size
      case KScheme.Sqrt => math.sqrt(model.size).toInt
      case KScheme.Value(k) => k
    }
    input.map(
      predict(model, knownColums, unknownColumns, _, metric, kv, weights)
    )
  }

}
