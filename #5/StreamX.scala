

object StreamX {
	def main(args: Array[String]) = {
		val a = Stream(1,2,3)
		println(a.scan(0)(_ - _).toList)
	}
}