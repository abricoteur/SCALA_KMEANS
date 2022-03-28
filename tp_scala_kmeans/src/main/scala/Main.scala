object Main :

  def main(args:Array[String]) : Unit =

    val data : Data = new Data("iris.data",",")
    data.init()

    val kmeans : Kmeans = new Kmeans(data, 2, 3)
    kmeans.init()