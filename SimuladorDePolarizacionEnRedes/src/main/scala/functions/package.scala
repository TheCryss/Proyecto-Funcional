package object functions {
  // dataTypes
  import dataTypes._

  // 2.1.1. rhoER
  //d:Distribution:(Frequency, DistributionValues):(Pi,Y)
  def rhoER(d:Distribution):Double = {
    // d._1:Pi  d._2:Y
    // K = 10
    val alfa = 1.6
    val k = d._2.length
    val sumRes = for (i <- (0 until k); j <- (0 until k)) yield
      Math.pow(d._1(i), 1+alfa) * d._1(j) * (d._2(i) - d._2(j)).abs
    10 * sumRes.sum
  }

  // 2.2.1. rho
  def rho(d_k:Discretizacion, sb:SpecificBeliefConf):Double = {
    def generarIntervalos(d:Discretizacion):Vector[(Double,Double)] = {
      if (d.length == 1) Vector((d(0), 1:Double))
      else Vector( (d(0), d(1)) ).concat(generarIntervalos(d.tail))
    }

    def generarPib(intervalos: Vector[(Double, Double)], sb: SpecificBeliefConf): Frequency = {
      //Numero de agentes
      val nags: Double = sb.length

      for (i <- intervalos) yield {
        val li: Double = i._1
        val ls: Double = i._2
        sb.count(s => (s >= li) && (s < ls)).toDouble / nags
      }
    }

    val intervalos = Vector( (0:Double, d_k(0)) ).concat(generarIntervalos(d_k))

    val yb = for (i <- intervalos) yield (i._1 + i._2)/2
    val pib = generarPib(intervalos, sb)

    rhoER(pib, yb)
  }
}