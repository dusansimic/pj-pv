object Program {

	def main(args: Array[String]) = {
		// zad1()
		// zad2()
		// zad3()
		// zad4()
		// zad5()
	}

	// 1. Definisati funkciju koja prima listu brojeva i ukoliko je
  //  ona parne duzine, svaki element kvadrira, inace svaki element
  //  mnozi sa 10. Koristiti funkciju map.
	def parNeparLista(l: List[Int]) : List[Int] = {
		l match {
			case l if l.length % 2 == 0 => l.map(n => n * n)
			case l => l.map(n => n * 10)
		}
	}

	def zad1() = {
		var l1: List[Int] = List(4, 6, 8, 2, 3, 1)
		var l2: List[Int] = List(4, 6, 8, 2, 3, 1, 2)
		
		println(parNeparLista(l1))
		println(parNeparLista(l2))
	}

	// 2. Napisati funkciju koja prima niz karaktera. Prvo je potrebno
  //  funkcijom filter izbaciti sva velika slova, a zatim pomocu funkcije
  //  map pretvoriti sva preostala slova u velika.
	def izbaciIPretvori(arr: Array[Char]) : Array[Char] = {
		arr.filter(c => !c.isUpper).map(c => if (c.isLower) c.toUpper else c)
	}

	def zad2() = {
		var arr: Array[Char] = Array('A', 'a', 'B', 'b', 'C', 'c', '1', '2', '!')

		for (c <- izbaciIPretvori(arr)) {
			println(c)
		}
	}

	// 3. Definisati funkciju prosecnaDuzina koja prima niz stringova
  //  i racuna njihovu prosecnu duzinu pomocu funkcije reduce i map.
	def prosecnaDuzina(arr: Array[String]) : Double = {
		var l = arr.length
		arr.map(s => s.length() * 1.0).reduce((a, b) => a + b) / l
	}

	def zad3() = {
		var arr: Array[String] = Array("Aaa", "Bbbbb", "Cc", "1111111", "2")

		println(prosecnaDuzina(arr))
	}


	// 4. Definisati funkciju koja prima niz brojeva. Zadrzi one brojeve
	//    kojima je prva i poslednja cifra ista, te svaki broj "okrene"
	//    po principu: "12341" -> "14321". Koristiti map i filter.
	def zadrziIste(arr: Array[Int]) : Array[Int] = {
		def okreni(a: Int) : Int = {
			_okreni(a, 0)
		}

		def _okreni(a: Int, acc: Int) : Int = {
			a match {
				case b if b == 0 => acc
				case b => _okreni(a / 10, acc * 10 + a % 10)
			}
		}

		arr.filter(n => (n % 10) == (okreni(n) % 10)).map(okreni)
	}

	def zad4() = {
		var arr: Array[Int] = Array(1234, 12341, 12345)

		for (n <- zadrziIste(arr)) {
			println(n)
		}
	}

	// 5. Definisati funkciju koja prima niz stringova. Treba da izbaciti
  //  sve one koji imaju vise od 4 velika slova. Nakon toga potrebno
  //  je na svaki od njih nalepiti sebe u "ogledalu" ("abc" -> 
  //  "abccba") i na kraju ih sve spojiti u jedan veliki string. 
  //  Koristiti map, filter, reduce.
	def izbaciIOblepi(arr: Array[String]) : String = {
		arr.filter(s => s.count(_.isUpper) <= 4).map(s => s + s.reverse).reduce((a, b) => a + b)
	}

	def zad5() = {
		var arr: Array[String] = Array("abc", "cbd", "BCADD")
		
		println(izbaciIOblepi(arr))
	}
}