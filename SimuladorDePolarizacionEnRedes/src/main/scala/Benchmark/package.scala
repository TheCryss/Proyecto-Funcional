import dataTypes._
import org.scalameter._
package object Benchmark {
  type Algoritmo = (Distribution) => Double
  type AlgoritmoPar = (DistributionPar) => Double

  def compararAlgoritmos(a1:Algoritmo, a2:AlgoritmoPar) (m1:Distribution, m2:DistributionPar):(Double,Double, Double) = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (a1(m1))

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (a2(m2))

    val speedUp= timeA1.value/timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }

  type test= (Discretizacion,SpecificBeliefConf)
  type Algoritmo2 = (Discretizacion,SpecificBeliefConf) => Double

  def testc(a1: Algoritmo2, a2: Algoritmo2) (m1: test, m2: test): (Double, Double, Double) = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer (new Warmer.Default) measure (a1 (m1._1,m1._2) )

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer (new Warmer.Default) measure (a2 (m2._1,m2._2) )

    val speedUp = timeA1.value / timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }

 // type test = (Discretizacion, SpecificBeliefConf)
  type Algoritmo3 = SpecificWeightedGraph => IndexedSeq[IndexedSeq[Double]]

  def testw(a1: Algoritmo3, a2: Algoritmo3)(m1: SpecificWeightedGraph): (Double, Double, Double) = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer (new Warmer.Default) measure (a1(m1))

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer (new Warmer.Default) measure (a2(m1))

    val speedUp = timeA1.value / timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }

  type Algoritmo4 = (SpecificBeliefConf,SpecificWeightedGraph) => SpecificBeliefConf

  def testb(a1: Algoritmo4, a2: Algoritmo4)(m1:SpecificBeliefConf,m2: SpecificWeightedGraph): (Double, Double, Double) = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer (new Warmer.Default) measure (a1(m1,m2))

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer (new Warmer.Default) measure (a2(m1,m2))

    val speedUp = timeA1.value / timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }
}
