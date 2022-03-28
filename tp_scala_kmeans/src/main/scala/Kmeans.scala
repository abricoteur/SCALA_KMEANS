import java.io.{File, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Kmeans(data : Data, choix : Int, k : Int) :

  var choix2 : Int = choix

  def init() : Unit =

    if(choix==2){choix2 += 1}else{choix2 -= 1}

    //initialisation aléatoire des centroids
    var centroids = new Array[Array[Double]](k)
    for i <-0 until k do
      centroids(i) = new Array[Double](2)
      centroids(i)(0) = Random.between(data.findMin(choix),data.findMax(choix))
      centroids(i)(1) = Random.between(data.findMin(choix2),data.findMax(choix2))
    var clusters = affectation(centroids)

    //création d'une copie des centroids
    var copie = new Array[Array[Double]](k)
    copie = centroids.map(_.map(identity))

    val pw = new PrintWriter(new File("points.txt"))

    //boucle d'actualisation des centroids
    //on test si les centroids changent entre l'étape n et n+1
    var etape = 0
    while (different(copie, centroids) || etape < 1) && etape < 20 do
      println("Cycle " + etape + " :")
      copie = centroids.map(_.map(identity))
      centroids = calcCentroids(clusters, centroids)
      for e <- centroids do
        println("Centroids " + centroids.indexOf(e) + " : x = " + e(0) + ", y = " + e(1))
      clusters = affectation(centroids)

      //On écrit les coordonnées des centroids et des points dans un fichier texte
      var s = ""
      for e <- clusters do
        for point <- e do
          s += point(0) + "," + point(1) + "," + clusters.indexOf(e) + ",\n"
      for e <- centroids do
        s += e(0) + "," + e(1) + ",\n"
      pw.write(s)
      pw.write("<-------->\n")

      etape+=1

    pw.close()

  //test si les deux tableaux en paramètre son différents
  def different(cp : Array[Array[Double]], centroids : Array[Array[Double]]) : Boolean =

    var test : Boolean = false
    for i <- centroids.indices do
      if cp(i)(0) != centroids(i)(0) || cp(i)(1) != centroids(i)(1) then
        test = true
    test

  //actualise les coordonnées des centroids
  def calcCentroids(clusters : ArrayBuffer[ArrayBuffer[Array[Double]]], centroids : Array[Array[Double]]) : Array[Array[Double]] =

    for i <- centroids.indices do
      val moyenne = data.moyenne(clusters(i))
      centroids(i)(0) = moyenne(0)
      centroids(i)(1) = moyenne(1)

    centroids

  //affecte les points aux clusters selon la distance aux centroids
  def affectation(centroids : Array[Array[Double]]) : ArrayBuffer[ArrayBuffer[Array[Double]]] =

    val clusters : ArrayBuffer[ArrayBuffer[Array[Double]]] = ArrayBuffer[ArrayBuffer[Array[Double]]]()
    for i <- centroids.indices do
      clusters += ArrayBuffer[Array[Double]]()

    for e <- data.data do
      clusters(cenMin(e(choix).toDouble, e(choix2).toDouble, centroids)) += Array(e(choix).toDouble, e(choix2).toDouble)

    clusters

  //retourne l'index du centroid le plus proche des coordonnées en paramètres
  def cenMin(a: Double, o : Double, centroids : Array[Array[Double]]) : Int =
    var min = scala.math.sqrt(scala.math.pow(a-centroids(0)(0), 2) + scala.math.pow(o-centroids(0)(1),2))
    var cen = centroids(0)
    for e <- centroids do
      if scala.math.sqrt(scala.math.pow(a-e(0), 2) + scala.math.pow(o-e(1),2)) < min then
        min = scala.math.sqrt(scala.math.pow(a-e(0), 2) + scala.math.pow(o-e(1),2))
        cen = e
    centroids.indexOf(cen)