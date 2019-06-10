package knn

import java.io.File
import scopt.OParser
import kantan.csv._
import kantan.csv.ops._

object Main {

  case class Options(
      command: Command = Command.Predict,
      modelFile: Option[File] = None,
      queryFile: Option[File] = None,
      outFile: Option[File] = None,
      inputColumns: Option[Seq[Int]] = None,
      outputColumns: Option[Seq[Int]] = None,
      separator: Char = ',',
      metric: Metric = Metrics.hassanat,
      normalize: Boolean = false,
      k: KScheme = KScheme.Sqrt,
      weights: Weighting = Weighting.Const,
      crossValidPartitions: Int = 10
  )

  val OptionParser = {
    val builder = OParser.builder[Options]
    import builder._
    OParser.sequence(
      programName("knn"),
      opt[File]('m', "model")
        .required()
        .validate(f => Either.cond(f.exists, (), s"File not found: $f"))
        .validate(f => Either.cond(f.isFile, (), s"$f is not a file"))
        .action((f, o) => o.copy(modelFile = Some(f)))
        .text("The csv file containing model data"),
      opt[File]('q', "query")
        .validate(f => Either.cond(f.exists, (), s"File not found: $f"))
        .validate(f => Either.cond(f.isFile, (), s"$f is not a file"))
        .action((f, o) => o.copy(queryFile = Some(f)))
        .text("The csv file containing query data for prediction"),
      opt[File]('o', "out")
        .action((f, o) => o.copy(outFile = Some(f)))
        .text("File where the prediction results will be stored"),
      opt[Seq[Int]]('k', "known-cols")
        .action((i, o) => o.copy(inputColumns = Some(i)))
        .text("Column indices of known data in the model"),
      opt[Seq[Int]]('u', "unknown-cols")
        .action((s, o) => o.copy(outputColumns = Some(s)))
        .text("Column indices of unknown data in the model"),
      opt[Char]('s', "separator")
        .action((c, o) => o.copy(separator = c))
        .text("Separator used in csv files (default is ',')"),
      opt[Unit]('n', "normalize")
        .action((c, o) => o.copy(normalize = true))
        .text("Normalize input data"),
      opt[KScheme]('k', "neighbour-count")
        .action((k, o) => o.copy(k = k))
        .text(
          "The parameter determining how many neighbours are considered during prediction (default: sqrt(n))"
        ),
      opt[Metric]('f', "metric")
        .action((f, o) => o.copy(metric = f))
        .text(
          s"Function used to determine distance between datapoints (${Metrics.byName.keys.mkString("|")})"
        ),
      opt[Weighting]('w', "weighting")
        .action((w, o) => o.copy(weights = w))
        .text("How neighbour weights are determined"),
      cmd("verify")
        .action((_, o) => o.copy(command = Command.Verify))
        .text("Check how well the predictor works using cross-validation")
        .children(
          opt[Int]("partitions")
            .action((p, o) => o.copy(crossValidPartitions = p))
            .text("Number of partitions to make")
        )
    )
  }

  def main(args: Array[String]): Unit = {
    OParser.parse(OptionParser, args, Options()).foreach { options =>
      val model = ReadResult
        .sequence(
          options.modelFile.get
            .asCsvReader[Vector[Double]](options.separator, false)
            .toVector
        )
        .fold(err => sys.error(err.toString), identity)

      val (known, unknown) =
        (options.inputColumns, options.outputColumns) match {
          case (Some(i), Some(o)) => (i.toVector, o.toVector)
          case (Some(i), None) =>
            (i.toVector, model.head.indices.toVector diff i)
          case (None, Some(o)) =>
            (model.head.indices.toVector diff o, o.toVector)
          case (None, None) =>
            (model.head.indices.init.toVector, Vector(model.head.indices.last))
        }

      options.command match {
        case Command.Predict =>
          val query = ReadResult
            .sequence(
              options.queryFile.get
                .asCsvReader[Vector[Double]](options.separator, false)
                .toVector
            )
            .fold(err => sys.error(err.toString), identity)

          val result =
            Prediction.predictAll(
              model,
              known,
              unknown,
              query,
              options.metric,
              options.k,
              options.weights
            )

          options.outFile.get
            .writeCsv[Vector[Double]](result, options.separator)
        case Command.Verify =>
          val partitionSize = model.size / options.crossValidPartitions
          val results = for {
            i <- 0 until options.crossValidPartitions
          } yield {
            val start = i * partitionSize
            val run = partitionSize min (model.size - start)
            val learn = model.patch(start, Seq.empty, run)
            val test = model.slice(start, start + run)
            val (input, expected) = test.map { row =>
              (known.map(row), unknown.map(row))
            }.unzip
            val res = Prediction.predictAll(
              learn,
              known,
              unknown,
              input,
              options.metric,
              options.k,
              options.weights
            )
            val squareErrors = (expected zip res).map {
              case (e, r) =>
                (e zip r).map { case (a, b) => (a - b) * (a - b) }.sum
            }
            val hitCount = (expected zip res).count {
              case (e, r) => e.map(_.toInt) == r.map(_.toInt)
            }
            (squareErrors, hitCount)
          }
          val squareErrors = results.flatMap(_._1)
          val mse = squareErrors.sum / squareErrors.size
          val hitPercentage = results
            .map(_._2)
            .sum
            .toDouble * 100.0 / model.size
          println(s"Mean square error: $mse")
          println(s"Exact hit ratio: $hitPercentage%")
      }
    }
  }

}
