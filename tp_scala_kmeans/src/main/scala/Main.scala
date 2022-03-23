object Main :

  def main(args:Array[String]) : Unit =

    val data : Data = new Data("iris.data",",")
    data.init()

    val kmeans : Kmeans = new Kmeans
    kmeans.init(data, 3, 2)

  //séparer Data.scala Iris.scala -> rendre Data.scala général
  //graphique couleur selon l'espèce 
  //faire en sorte que s'arrete lors de convergence -> tester la distance entre un meme centroid apres et avant recalcul