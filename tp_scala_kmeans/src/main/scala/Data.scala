import scala.collection.mutable.ArrayBuffer
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

	def moyenne(points : ArrayBuffer[Array[Double]]) : Array[Double] =

		var moyenneAbs = 0.0
		var moyenneOrd = 0.0
		for point <- points do
			moyenneAbs += point(0)
			moyenneOrd += point(1)
		if moyenneAbs != 0.0 then
			moyenneAbs /= points.length
		if moyenneOrd != 0.0 then
			moyenneOrd /= points.length
		Array(moyenneAbs, moyenneOrd)

