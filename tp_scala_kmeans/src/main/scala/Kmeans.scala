import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Kmeans() :

  def init(data : Data, k : Int, choix : Int) : Unit =
    //-> choisir k points aléatoires (centroid)
    var centroids = new Array[Double](k)
    for i <-0 until k do
      centroids(i) = Random.between(data.findMin(choix),data.findMax(choix))

    var clusters = affectation(data, centroids, choix)
    for etape <- 0 until k do
      println(etape)
      println(centroids.mkString("Array(", ", ", ")"))
      for e <- clusters do
        println(e.mkString("Array(", ", ", ")"))
      centroids = calcCentroids(clusters, centroids)
      clusters = affectation(data, centroids, choix)

  def calcCentroids(clusters : ArrayBuffer[ArrayBuffer[Double]], centroids : Array[Double]) : Array[Double] =

    for i <- centroids.indices do
      var moyenne : Double = 0.0
      for e <- clusters(i) do
        moyenne += e
      moyenne /= clusters(i).length
      centroids(i) = moyenne //moyenne des valeurs de clusters(i)

    centroids


  def affectation(data : Data, centroids : Array[Double], choix : Int) : ArrayBuffer[ArrayBuffer[Double]] =
    //Affecter chaque point (élément de la matrice de donnée) au groupe dont il est le plus proche au son centre
    //Recalculer le centre de chaque cluster  et modifier le centroid
    //jusque convergence

    val clusters : ArrayBuffer[ArrayBuffer[Double]] = ArrayBuffer[ArrayBuffer[Double]]()
    for i <- centroids.indices do
      clusters += ArrayBuffer[Double]()

    for e <- data.data do
      clusters(cenMin(e(choix).toDouble, centroids)) += e(choix).toDouble

    clusters

  //return nearest centroid index
  def cenMin(d: Double, centroids : Array[Double]) : Int =
    var min = scala.math.pow(d - centroids(0), 2)
    var cen = centroids(0)
    for e <- centroids do
      if scala.math.pow(d - e, 2) < min then
        min = scala.math.pow(d - e, 2)
        cen = e
    centroids.indexOf(cen)