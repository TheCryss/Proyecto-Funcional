import Benchmark._
import functions._
import dataTypes._

import scala.collection.parallel.immutable._
import org.scalameter._
//import Benchmark._

import scala.collection.parallel.CollectionConverters.VectorIsParallelizable

// Pruebas funcion rhoER

val pi1 = Vector(0.4, 0.6)
val pi1p = ParVector(0.4, 0.6)
val pi2 = Vector(0.5, 0.5)
val pi2p = ParVector(0.5, 0.5)
val pi3 = Vector(0.6, 0.4)
val pi4 = Vector(0.1, 0.9)
val pi5 = Vector(0.9, 0.1)

val y = Vector(1.0, 5.0)
val yp = ParVector(1.0, 5.0)

def distribucion (n:Int):(Vector[Double],Vector[Double]) ={
  val frecuencias = for(i<- 0 until n) yield Math.random()
  val decisiones = for(i<- 0 until n) yield i.toDouble
  (frecuencias.toVector,decisiones.toVector)
}


val test1 = distribucion(10)
val test1p= ((test1._1).par,(test1._2).par)

val test2 = distribucion(50)
val test2p= ((test2._1).par,(test2._2).par)

val test3 = distribucion(100)
val test3p= ((test3._1).par,(test3._2).par)

val test4 = distribucion(500)
val test4p= ((test4._1).par,(test4._2).par)

val test5 = distribucion(1000)
val test5p= ((test5._1).par,(test5._2).par)
//rhoER(test1)
//rhoERPar(test1p)
/*
rhoERPar((pi1p,yp))
rhoER((pi2,y))
rhoERPar((pi2p,yp))
rhoER((pi3,y))
rhoER((pi4,y))
rhoER((pi5,y))


compararAlgoritmos(rhoER,rhoERPar)(test1,test1p)
compararAlgoritmos(rhoER,rhoERPar)(test2,test2p)
compararAlgoritmos(rhoER,rhoERPar)(test3,test3p)
compararAlgoritmos(rhoER,rhoERPar)(test4,test4p)
compararAlgoritmos(rhoER,rhoERPar)(test5,test5p)
*/




//Pruebas funcion rho

val d1 = Vector(0.2, 0.4, 0.6, 0.8)
val d2 = Vector(0.1, 0.4, 0.7, 0.9)

def b0(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i:Int) => {0.6})
}

def b1(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i: Int) => {if (i <= nags / 2) 0.6 else 0.4})
}

def b2(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i:Int)=> if (i <= nags/2) 0.3 else 0.9)
}

def b3(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i:Int) => (i+1).toDouble/nags.toDouble)
}

val b0_10= b0(10)
val b0_20 = b0(20)

val b1_10= b1(50)
val b1_20 = b1(50)

val b2_10= b2(100)
val b2_20 = b2(100)

val b3_10= b3(1000)
val b3_20 = b3(1000)

/*******************COMPARANDO ALGORiTMO********************/
/*
testc (rho,rhoPar)((test1._1,b3_20),(test1._1,b3_20))
testc (rho,rhoPar)((test2._1,b3_20),(test2._1,b3_20))
testc (rho,rhoPar)((test3._1,b3_20),(test3._1,b3_20))
testc (rho,rhoPar)((test4._1,b3_20),(test4._1,b3_20))
testc (rho,rhoPar)((test5._1,b3_20),(test5._1,b3_20))
*/
//compararAlgoritmos(rhoER,rhoERPar)(test2,test2p)


//rho(test1._1, b1_10)
//rhoPar(test1._1,b1_10)

/*
rho(d1, b1_10)
rhoPar(d1,b1_10)

rho(d1, b2_10)
rhoPar(d1, b2_10)
rho(d2, b1_10)
rho(d2, b2_10)
rho(d1, b3_10)
rho(d2, b3_10)
*/


//Pruebas funcion showWeightedGraph

def i1(nags:Int):SpecificWeightedGraph = {
  ((i:Int, j:Int) => if (i==j) 1.0
  else if (i<j) 1.0/(j-i).toDouble
  else 0.0, nags)
}

def i2(nags:Int):SpecificWeightedGraph = {
  ((i:Int, j:Int) => if (i==j) 1.0
  else if (i<j) (j-i).toDouble/nags.toDouble
  else (nags-(i-j)).toDouble/nags.toDouble, nags)
}

val i1_10 = i1(10)
val i2_10 = i2(10)
val i1_20 = i1(20)
val i2_20 = i2(20)
/************************* RENDIMIENTO **************************/
val i1_10 = i1(10)
val i1_20 = i1(50)
val i1_30 = i1(100)
val i1_40 = i1(500)
val i1_50 = i1(1000)
/*
testw(showWeightedGraph,showWeightedGraphPar)(i1_10)
testw(showWeightedGraph,showWeightedGraphPar)(i1_20)
testw(showWeightedGraph,showWeightedGraphPar)(i1_30)
testw(showWeightedGraph,showWeightedGraphPar)(i1_40)
testw(showWeightedGraph,showWeightedGraphPar)(i1_50)

*/
/*

showWeightedGraph(i1_10)
showWeightedGraphPar(i1_10)
showWeightedGraph(i2_10)
showWeightedGraphPar(i2_10)
*/

/************************* RENDIMIENTO **************************/
testb(confBiasUpdate,confBiasUpdatePar)(b0_10,i1_10)
testb(confBiasUpdate,confBiasUpdatePar)(b1_10,i1_30)
testb(confBiasUpdate,confBiasUpdatePar)(b2_10,i1_40)
testb(confBiasUpdate,confBiasUpdatePar)(b3_10,i1_50)

// Pruebas funcion confBiasUpdate
/*
confBiasUpdate(b1_10, i1_10)
confBiasUpdatePar(b1_10, i1_10)
confBiasUpdate(b1_10, i2_10)
confBiasUpdatePar(b1_10, i2_10)
confBiasUpdate(b2_10, i1_10)
confBiasUpdatePar(b2_10, i1_10)
confBiasUpdate(b2_10, i2_10)
confBiasUpdatePar(b2_10, i2_10)
confBiasUpdate(b3_10, i1_10)
confBiasUpdatePar(b3_10, i1_10)
confBiasUpdate(b3_10, i2_10)
confBiasUpdatePar(b3_10, i2_10)
*/


// Pruebas funcion simulate
for {
  b <- simulate(confBiasUpdate, i1_10, b1_10, 10)
} yield (b, rho(d1, b))
