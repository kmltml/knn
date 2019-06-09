package knn

import breeze.linalg.{ norm, DenseMatrix }

import scala.math.{ abs, max, min, sqrt }

object Metrics {

  val byName: Map[String, Metric] = Map(
    "average" -> average,
    "chebyshev" -> chebyshev,
    "cosine" -> cosine,
    "euclidean" -> euclidean,
    "hassanat" -> hassanat,
    "manhattan" -> manhattan,
    "squaredChiSquared" -> squaredChiSquared
  )

  def average(a: Vector[Double], b: Vector[Double]): Double =
    sqrt((a zip b).map { case (ai, bi) => (ai - bi) * (ai - bi) }.sum) / a.length

  def chebyshev(a: Vector[Double], b: Vector[Double]): Double =
    (a zip b).map { case (ai, bi) => abs(ai - bi) }.max

  def cosine(a: Vector[Double], b: Vector[Double]): Double = {
    val dotProduct = (a zip b).map { Function.tupled(_ * _) }.sum
    val aNorm = sqrt(a.map(ai => ai * ai).sum)
    val bNorm = sqrt(b.map(bi => bi * bi).sum)
    1 - dotProduct / (aNorm * bNorm)
  }

  def euclidean(a: Vector[Double], b: Vector[Double]): Double =
    sqrt((a zip b).map { case (ai, bi) => (ai - bi) * (ai - bi) }.sum)

  def hassanat(a: Vector[Double], b: Vector[Double]): Double =
    (a zip b).map {
      case (ai, bi) if min(ai, bi) >= 0 =>
        1 - (1 + min(ai, bi)) / (1 + max(ai, bi))
      case (ai, bi) =>
        1 - (1 + min(ai, bi) + abs(min(ai, bi))) / (1 + max(ai, bi) + abs(
          min(ai, bi)
        ))
    }.sum

  def manhattan(a: Vector[Double], b: Vector[Double]): Double =
    (a zip b).map { case (ai, bi) => abs(ai - bi) }.sum

  def squaredChiSquared(a: Vector[Double], b: Vector[Double]): Double =
    (a zip b).map { case (ai, bi) => (ai - bi) * (ai - bi) / abs(ai - bi) }.sum

}
