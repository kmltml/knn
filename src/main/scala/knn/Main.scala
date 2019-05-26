package knn

import java.io.File
import scopt.OParser

object Main {

  case class Options(
      modelFile: Option[File] = None,
      queryFile: Option[File] = None,
      outFile: Option[File] = None,
      inputColumns: Option[Seq[Int]] = None,
      outputColumns: Option[Seq[Int]] = None
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
        .required()
        .validate(f => Either.cond(f.exists, (), s"File not found: $f"))
        .validate(f => Either.cond(f.isFile, (), s"$f is not a file"))
        .action((f, o) => o.copy(queryFile = Some(f)))
        .text("The csv file containing query data for prediction"),
      opt[File]('o', "out")
        .required()
        .action((f, o) => o.copy(outFile = Some(f)))
        .text("File where the prediction results will be stored"),
      opt[Seq[Int]]('k', "known-cols")
        .action((i, o) => o.copy(inputColumns = Some(i)))
        .text("Column indices of known data in the model"),
      opt[Seq[Int]]('u', "unknown-cols")
        .action((s, o) => o.copy(outputColumns = Some(s)))
        .text("Column indices of unknown data in the model")
    )
  }

  def main(args: Array[String]): Unit = {
    OParser.parse(OptionParser, args, Options()).foreach { options =>
      println(options)
    }
  }

}
