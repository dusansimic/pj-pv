object AppMain {

	def main( args: Array[String] ) : Unit = {
		println("Hello world!")

		var res = if (true) {
			2
		} else {
			5
		}

		println(zadatak1())
	}

	def zadatak1() : Double = {
		// return 2.0
		println("Vraticu broj 2!")
		2.0
	}

}
