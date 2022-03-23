import scala.io.{BufferedSource, Source}
import scala.math.sqrt

class Data(file : String, separator : String) :

	val source: BufferedSource = Source.fromFile(file)
	//var a = source.getLines.length
	var data : Array[Array[String]] = Array.ofDim[String](150, 5)

	//extract data from file into multi dim array
	def init() : Unit =
		//val cols = source.getLines().take(1).split(this.separator).length
		var i = 0
		for (line <- source.getLines()) do
			var j = 0
			for ( e <- line.split(separator)) do
				this.data(i)(j) = e
				j+=1
			i+=1
		source.close()

	//return max value of choix in data
	def findMax(choix : Int) : Double =
		var max = data(0)(choix).toDouble
		for e <- data do
			if e(choix).toDouble > max then
				max = e(choix).toDouble
		max

	//return min value of choix in data
	def findMin(choix : Int) : Double =
		var min = data(0)(choix).toDouble
		for e <- data do
			if e(choix).toDouble < min then
				min = e(choix).toDouble
		min

	def moyennes() : Array[Array[Double]] =

		val tabSetosa = Array(0.0, 0.0, 0.0, 0.0)
		val tabVersicolor = Array(0.0, 0.0, 0.0, 0.0)
		val tabVirginica = Array(0.0, 0.0, 0.0, 0.0)
		var setosaCount = 0
		var versicolorCount = 0
		var virginicaCount = 0
	
		for e <- data do
			
			e(4) match 
				
				case "Iris-setosa" =>   tabSetosa(0) = tabSetosa(0) + e(0).toDouble
							tabSetosa(1) += e(1).toDouble
							tabSetosa(2) += e(2).toDouble
							tabSetosa(3) += e(3).toDouble
							setosaCount+=1
				
				case "Iris-versicolor" => tabVersicolor(0) += e(0).toDouble
							  tabVersicolor(1) += e(1).toDouble
							  tabVersicolor(2) += e(2).toDouble
							  tabVersicolor(3) += e(3).toDouble
							  versicolorCount+=1
				
				case "Iris-virginica" => tabVirginica(0) += e(0).toDouble
							 tabVirginica(1) += e(1).toDouble
							 tabVirginica(2) += e(2).toDouble
							 tabVirginica(3) += e(3).toDouble
							 virginicaCount+=1

		tabSetosa(0) /= setosaCount
		tabSetosa(1) /= setosaCount
		tabSetosa(2) /= setosaCount
		tabSetosa(3) /= setosaCount

		tabVersicolor(0) /= versicolorCount
		tabVersicolor(1) /= versicolorCount
		tabVersicolor(2) /= versicolorCount
		tabVersicolor(3) /= versicolorCount
		
		tabVirginica(0) /= virginicaCount
		tabVirginica(1) /= virginicaCount
		tabVirginica(2) /= virginicaCount
		tabVirginica(3) /= virginicaCount

		Array(tabSetosa,tabVersicolor,tabVirginica)

	def ecartVariance(param : String) : Array[Array[Double]] =

		val tabSetosa = Array(0.0, 0.0, 0.0, 0.0)
		val tabVersicolor = Array(0.0, 0.0, 0.0, 0.0)
		val tabVirginica = Array(0.0, 0.0, 0.0, 0.0)
		var setosaCount = 0
		var versicolorCount = 0
		var virginicaCount = 0

		val moyennes = this.moyennes()

		for e <- data do

			e(4) match

				case "Iris-setosa" =>  tabSetosa(0) += scala.math.pow(moyennes(0)(0) - e(0).toDouble, 2)
					tabSetosa(1) += scala.math.pow(moyennes(0)(1) - e(1).toDouble, 2)
					tabSetosa(2) += scala.math.pow(moyennes(0)(2) - e(2).toDouble, 2)
					tabSetosa(3) += scala.math.pow(moyennes(0)(3) - e(3).toDouble, 2)
					setosaCount+=1

				case "Iris-versicolor" => tabVersicolor(0) += scala.math.pow(moyennes(1)(0) - e(0).toDouble, 2)
					tabVersicolor(1) += scala.math.pow(moyennes(1)(1) - e(1).toDouble, 2)
					tabVersicolor(2) += scala.math.pow(moyennes(1)(2) - e(2).toDouble, 2)
					tabVersicolor(3) += scala.math.pow(moyennes(1)(3) - e(3).toDouble, 2)
					versicolorCount+=1

				case "Iris-virginica" => tabVirginica(0) += scala.math.pow(moyennes(2)(0) - e(0).toDouble, 2)
					tabVirginica(1) += scala.math.pow(moyennes(2)(1) - e(1).toDouble, 2)
					tabVirginica(2) += scala.math.pow(moyennes(2)(2) - e(2).toDouble, 2)
					tabVirginica(3) += scala.math.pow(moyennes(2)(3) - e(3).toDouble, 2)
					virginicaCount+=1

		tabSetosa(0) /= setosaCount
		tabSetosa(1) /= setosaCount
		tabSetosa(2) /= setosaCount
		tabSetosa(3) /= setosaCount

		tabVersicolor(0) /= versicolorCount
		tabVersicolor(1) /= versicolorCount
		tabVersicolor(2) /= versicolorCount
		tabVersicolor(3) /= versicolorCount

		tabVirginica(0)  /= virginicaCount
		tabVirginica(1)  /= virginicaCount
		tabVirginica(2)  /= virginicaCount
		tabVirginica(3)  /= virginicaCount

		if(param == "variance") then
			return Array(tabSetosa,tabVersicolor,tabVirginica)

		tabSetosa(0)  = sqrt(tabSetosa(0))
		tabSetosa(1)  = sqrt(tabSetosa(0))
		tabSetosa(2)  = sqrt(tabSetosa(0))
		tabSetosa(3)  = sqrt(tabSetosa(0))

		tabVersicolor(0) = sqrt(tabVersicolor(0))
		tabVersicolor(1) = sqrt(tabVersicolor(1))
		tabVersicolor(2)  = sqrt(tabVersicolor(2))
		tabVersicolor(3)  = sqrt(tabVersicolor(3))

		tabVirginica(0)  = sqrt(tabVirginica(0))
		tabVirginica(1)  = sqrt(tabVirginica(1))
		tabVirginica(2)  = sqrt(tabVirginica(2))
		tabVirginica(3)  = sqrt(tabVirginica(3))

		Array(tabSetosa,tabVersicolor,tabVirginica)


	def variance() : Array[Array[Double]] =

		val tabSetosa = Array(0.0, 0.0, 0.0, 0.0)
		val tabVersicolor = Array(0.0, 0.0, 0.0, 0.0)
		val tabVirginica = Array(0.0, 0.0, 0.0, 0.0)
		var setosaCount = 0
		var versicolorCount = 0
		var virginicaCount = 0

		val moyennes = this.moyennes()

		for e <- data do

			e(4) match

				case "Iris-setosa" =>  tabSetosa(0) += scala.math.pow(moyennes(0)(0) - e(0).toDouble, 2)
					tabSetosa(1) += scala.math.pow(moyennes(0)(1) - e(1).toDouble, 2)
					tabSetosa(2) += scala.math.pow(moyennes(0)(2) - e(2).toDouble, 2)
					tabSetosa(3) += scala.math.pow(moyennes(0)(3) - e(3).toDouble, 2)
					setosaCount+=1

				case "Iris-versicolor" => tabVersicolor(0) += scala.math.pow(moyennes(1)(0) - e(0).toDouble, 2)
					tabVersicolor(1) += scala.math.pow(moyennes(1)(1) - e(1).toDouble, 2)
					tabVersicolor(2) += scala.math.pow(moyennes(1)(2) - e(2).toDouble, 2)
					tabVersicolor(3) += scala.math.pow(moyennes(1)(3) - e(3).toDouble, 2)
					versicolorCount+=1

				case "Iris-virginica" => tabVirginica(0) += scala.math.pow(moyennes(2)(0) - e(0).toDouble, 2)
					tabVirginica(1) += scala.math.pow(moyennes(2)(1) - e(1).toDouble, 2)
					tabVirginica(2) += scala.math.pow(moyennes(2)(2) - e(2).toDouble, 2)
					tabVirginica(3) += scala.math.pow(moyennes(2)(3) - e(3).toDouble, 2)
					virginicaCount+=1

		tabSetosa(0)  = sqrt(tabSetosa(0)/setosaCount)
		tabSetosa(1)  = sqrt(tabSetosa(0)/setosaCount)
		tabSetosa(2)  = sqrt(tabSetosa(0)/setosaCount)
		tabSetosa(3)  = sqrt(tabSetosa(0)/setosaCount)

		tabVersicolor(0) = sqrt(tabVersicolor(0)/versicolorCount)
		tabVersicolor(1) = sqrt(tabVersicolor(1)/versicolorCount)
		tabVersicolor(2)  = sqrt(tabVersicolor(2)/versicolorCount)
		tabVersicolor(3)  = sqrt(tabVersicolor(3)/versicolorCount)

		tabVirginica(0)  = sqrt(tabVirginica(0)/virginicaCount)
		tabVirginica(1)  = sqrt(tabVirginica(1)/virginicaCount)
		tabVirginica(2)  = sqrt(tabVirginica(2)/virginicaCount)
		tabVirginica(3)  = sqrt(tabVirginica(3)/virginicaCount)

		Array(tabSetosa,tabVersicolor,tabVirginica)


