package object functions {
  // dataTypes
  import dataTypes._

  // rhoER
  def rhoER(d:Distribution):Double = {
    // d._1:Pi  d._2:Y
    // K = 10
    val alfa = 1.6
    val k = d._2.length
    val sumRes = for (i <- (0 until k); j <- (0 until k)) yield
      Math.pow(d._1(i), 1+alfa) * d._1(j) * (d._2(i) - d._2(j)).abs
    10 * sumRes.sum
  }
}
