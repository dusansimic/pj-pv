object Program {

	def main(args: Array[String]) = {
		println(izbaciManje5(List(List(1,2,3), List(4,5,6), List(7,8,9, 10, 11, 12, 13, 14, 15, 16))))
	}

	// 0.
  //  0.1. Istampati sve brojeve u opsegu [1..10]
  //  0.2. Istampati sve brojeve u ospegu [1..10)
  //  0.3 Istampati sve brojeve u opsegu [1..100] da su deljivi sa 3
	def istampajSve() = {
		for (i <- 1 to 10) println(i)
		for (i <- 1 until 10) println(i)
		for (i <- 1 to 100 if i % 3 == 0) println(i)
	}

	// 1. Napisati funkciju "ispeglaj" koja prima listu ciji su elementi
  //  liste List[List[Any]], a vraca listu brojeva List[Any] tako sto sve elemente
  //  podlisti spoji u jednu listu. (Koristiti reduce)

  //  [[1, 2, 3], [4, 5], [6], [7, 8], [], [9]] -> [1, 2, 3, 4, 5, 6, 7, 8, 9]
	def ispeglaj(l: List[List[Any]]) : List[Any] = {
		l.reduce((a, b) => a ::: b)
	}

	// 2. Napisati funkciju koja prima listu listi brojeva List[List[Int]], i vraca
  //  listu suma elemenata listi koje ona sadrzi. Koristiti map i reduce.

  //  [[1, 2, 3], [2, 3], [1, 2, 4], [5]] -> [6, 5, 7, 5]
	def sumiraj(l: List[List[Int]]) : List[Int] = {
		l.filter(ll => ll != Nil).map(ll => ll.reduce((a, b) => a + b))
	}

	// 3. Napisati funkciju koja prima listu ciji su elementi liste brojeva,
  //  te iz svake podliste izbacuje parne brojeve. Ako slucajno ostane
  //  neka prazna lista, nju je potrebno izbaciti. Koristiti map i 
  //  filter

  //  [[1, 2, 3], [2, 4], [3, 4, 5], [7]] -> [[1, 3], [3, 5], [7]]
	def izbaciParne(l: List[List[Int]]) : List[List[Int]] = {
		l.map(ll => ll.filter(n => n % 2 != 0)).filter(ll => ll != Nil)
	}

	// 4. Napisati funkciju "okreni" koja prolazi kroz listu stringova, 
  //  te pomocu funkcija map i reverse "okrene" sve stringove.

  //  "neki string" -> "gnirts iken"

  //  ["abc", "dfg", "ert"] -> ["cba", "gfd", "tre"]
	def okreni(l: List[String]) : List[String] = {
		l.map(s => s.reverse)
	}

	// 5. Napisati funkciju koja prihvata listu listi brojeva [[Int]] i 
  //  iz svake podliste izbacuje elemente deljive sa 3.
	def izbaciDeljive(l: List[List[Int]]) : List[List[Int]] = {
		l.map(ll => ll.filter(n => n % 3 != 0))
	}

	// 6. Napisati funkciju koja prihvata listu listi brojeva, primenjuje na 
  //  nju prethodnu funkciju i onda izbaci sve podliste koje imaju manje 
  //  od 5 elemenata.
	def izbaciManje5(l: List[List[Int]]) : List[List[Int]] = {
		izbaciDeljive(l).filter(ll => ll.length >= 5)
	}
}