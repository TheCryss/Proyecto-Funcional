import scala.collection.parallel.immutable._
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable


package object functions {
  // dataTypes

  import dataTypes._

  // 2.1.1. rhoER
  //d:Distribution:(Frequency, DistributionValues):(Pi,Y)
  def rhoER(d: Distribution): Double = {
    // d._1:Pi  d._2:Y
    // K = 10
    val alfa = 1.6
    val k = d._2.length
    val sumRes = for (i <- (0 until k); j <- (0 until k)) yield
      Math.pow(d._1(i), 1 + alfa) * d._1(j) * (d._2(i) - d._2(j)).abs
    10 * sumRes.sum
  }

  // 2.2.1. rho
  def rho(d_k: Discretizacion, sb: SpecificBeliefConf): Double = {
    def generarIntervalos(d: Discretizacion): Vector[(Double, Double)] = {
      if (d.length == 1) Vector((d(0), 1: Double))
      else Vector((d(0), d(1))).concat(generarIntervalos(d.tail))
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

    // se concatena el primer intervalo que empiza en 0a
    val intervalos = Vector((0: Double, d_k(0))).concat(generarIntervalos(d_k))
    //Posibles decisiones que puede tomar la sociedad medida en intervalos
    val yb = for (i <- intervalos) yield (i._1 + i._2) / 2
    //Porcentaje de agentes dispuestos a tomar una decicio en cierto intervalo (posibilidad)
    val pib = generarPib(intervalos, sb)

    rhoER(pib, yb)
  }

  // 2.3.1. showWeightedGraph
  def showWeightedGraph(swg: SpecificWeightedGraph): IndexedSeq[IndexedSeq[Double]] = {
    val nags = swg._2 //# de agentes
    val wg = swg._1 //weighted graph
    val ags = 0 until nags
    for (i <- ags) yield for (j <- ags) yield wg(i, j)
  }

  // 2.3.2. confBiasUpdate
  def confBiasUpdate(b: SpecificBeliefConf, swg: SpecificWeightedGraph): SpecificBeliefConf = {
    val nags = b.length //numero de agentes
    val ags = 0 until nags //agentes creado como rango
    val wg = swg._1
    (for (i <- ags) yield {
      val ai = for (j <- ags; if (wg(j, i) > 0)) yield j
      val sumj_ai = for (j <- ai) yield {
        val bij = 1.0 - (b(j) - b(i)).abs
        val iji = wg(j, i)
        val bj_bi = b(j) - b(i)
        bij * iji * bj_bi
      }
      b(i) + (sumj_ai.sum / ai.length)
    }).toVector
  }

  // 2.3.3. simulate
  def simulate(fu: FunctionUpdate, swg: SpecificWeightedGraph,
               b0: SpecificBeliefConf, t: Int): IndexedSeq[SpecificBeliefConf] = {
    if (t == 0) IndexedSeq()
    else {
      val b1 = fu(b0, swg)
      IndexedSeq(b0, b1).concat(simulate(fu, swg, b1, t - 1))
    }
  }

  //******************************* VERSION PARALELA *******************************//
  def rhoERPar(d: DistributionPar): Double = {
    val alfa = 1.6
    val k = d._2.length
    val sumRes = for (i <- (0 until k); j <- (0 until k)) yield
      Math.pow(d._1(i), 1 + alfa) * d._1(j) * (d._2(i) - d._2(j)).abs
    10 * sumRes.sum
  }

  def rhoPar(d_k: Discretizacion, sb: SpecificBeliefConf): Double = {
    def generarIntervalos(d: Discretizacion): ParVector[(Double, Double)] = {
      if (d.length == 1) ParVector((d(0), 1: Double))
      else ParVector((d(0), d(1)))++(generarIntervalos(d.tail))
    }

    def generarPib(intervalos: ParVector[(Double, Double)], sb: SpecificBeliefConf): FrequencyPar = {
      //Numero de agentes
      val nags: Double = sb.length

      for (i <- intervalos) yield {
        val li: Double = i._1
        val ls: Double = i._2
        sb.count(s => (s >= li) && (s < ls)).toDouble / nags
      }
    }

    // se concatena el primer intervalo que empiza en 0a
    val intervalos = ParVector((0: Double, d_k(0)))++(generarIntervalos(d_k))
    //Posibles decisiones que puede tomar la sociedad medida en intervalos
    val yb = for (i <- intervalos) yield (i._1 + i._2) / 2
    //Porcentaje de agentes dispuestos a tomar una decicio en cierto intervalo (posibilidad)
    val pib = generarPib(intervalos, sb)

    rhoERPar(pib,yb)
  }

  def showWeightedGraphPar(swg: SpecificWeightedGraph): IndexedSeq[IndexedSeq[Double]] = {
    val nags = swg._2 //# de agentes
    val wg = swg._1 //weighted graph
    val ags = 0 until nags
    ags.par.map(i => ags.map(j => wg(i, j))).toIndexedSeq
  }

  def confBiasUpdatePar(b: SpecificBeliefConf, swg: SpecificWeightedGraph): SpecificBeliefConf = {
    val nags = b.length //numero de agentes
    val ags = (0 until nags) //agentes creado como rango
    val wg = swg._1
    ags.par.map { i =>
      val ai = for (j <- ags; if (wg(j, i) > 0)) yield j
      val sumj_ai = for (j <- ai) yield {
        val bij = 1.0 - (b(j) - b(i)).abs
        val iji = wg(j, i)
        val bj_bi = b(j) - b(i)
        bij * iji * bj_bi
      }
      b(i) + (sumj_ai.sum / ai.length)
    }.par.toVector
  }
}