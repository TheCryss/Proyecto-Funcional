package object dataTypes {
  // Tipo para los valores, reales, de una distribucion
  // Contiene cualquier valor real
  type DistributionValues = Vector[Double]

  // pi_k es una frecuencia de longitud k
  // si pi_k.lenght = k, 0 <= pi_k(i) <= 1, 0 <= i <= k-1
  // pi_k.sum == 1
  type Frequency = Vector[Double] //Valores entre 0 y 1
}