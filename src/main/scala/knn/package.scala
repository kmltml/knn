package object knn {

  type Metric = (Vector[Double], Vector[Double]) => Double

  implicit val metricOptionReader: scopt.Read[Metric] =
    scopt.Read.reads { s =>
      Metrics.byName.getOrElse(
        s,
        throw new IllegalArgumentException(s"Unknown metric $s")
      )
    }

}
