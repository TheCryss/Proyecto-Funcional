package object dataTypes {
  // Tipo para los valores, reales, de una distribucion
  // Contiene cualquier valor real
  type DistributionValues = Vector[Double]

  // pi_k es una frecuencia de longitud k
  // si pi_k.lenght = k, 0 <= pi_k(i) <= 1, 0 <= i <= k-1
  // pi_k.sum == 1
  type Frequency = Vector[Double] //Valores entre 0 y 1

  // Distribucion sobre Y
  // Frequency:Pi
  // DistributionValues: Y
  type Distribution = (Frequency, DistributionValues)

  // b:SpecificBeliefConf
  // 0 <= b(i) <= 1
  // b(i) indica cuanto cree el agente i
  // en la veracidad de la proposicion p
  // b.length = numero de agentes
  type SpecificBeliefConf = Vector[Double]

  // gb:GenericBeliefConf
  // gb(n) = b
  type GenericBeliefConf = Int => SpecificBeliefConf

  // d_k es una discretizacion de longitud k
  // del intervalo [0,1]
  type Discretizacion = Vector[Double]
}