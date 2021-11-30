object Program {

	def main(args: Array[String]) = {
	}

	/*
		1. Napisati funkciju koja prebrojava koliko elementa niza 
		   je parno.
	*/
	def prebroj(arr: Array[Int]) : Int = {
		var c = 0
		for (el <- arr)
			c += (el + 1) % 2
		c
	}

	/*
		2. Napisati funkciju koja prihvata niz string-ova i vraca 
		   samo one koji su duzi od nekog n.
	*/
	def vratiDuze(arr: Array[String], n: Int) : Array[String] = {
		var ret = Array[String]()
		for (s <- arr) {
			if (s.length() > n)
				ret = ret.appended(s)
		}
		ret
	}

	def vratiDuzeForComprehend(arr: Array[String], n: Int): Array[String] =
		for (s <- arr if s.length() > n) yield s

	/*
		3. Napisati funkciju koja prima niz brojeva i vraca sve kombinacije
		   parova brojeva tog niza (Tuple2[Int, Int]). Nakon toga, definisati
		   funkciju koja iz niza parova vraca samo one cija srednja vrednost
		   upada u neki interval [a..b].
	*/
	def vratiKombinacije(arr: Array[Int]) : Array[Tuple2[Int, Int]] =
		for (el1 <- arr; el2 <- arr) yield (el1, el2)

	def srednjaUpada(tpls: Array[Tuple2[Int, Int]], interval: Tuple2[Int, Int]) : Array[Tuple2[Int, Int]] = {
		var ret = Array[Tuple2[Int, Int]]()
		val a = interval._1
		val b = interval._2
		for (tpl <- tpls) {
			val avg = (tpl._1 + tpl._2) / 2
			if (avg >= a && avg <= b)
				ret = ret.appended(tpl)
		}
		ret
	}

	/*
		4. Napisati funkciju koja vraca skalarni proizvod dva niza.
	*/
	def skalarni(v1: Array[Int], v2: Array[Int]) : Int = {
		if (v1.length != v2.length)
			0
		else {
			var sum = 0
			for ((c1, c2) <- v1 zip v2) {
				sum += c1 * c2
			}
			sum
		}
	}

	/*
		5. Napisati funkciju koja prebrojava koliko elemenata liste 
		   l je manje od nekog n.
	*/
	def kolikoJeManje(l: List[Int], n: Int) : Int = {
		l match {
			case Nil => 0
			case (h: Int) :: (t: List[Int]) =>
				if (h < n)
					1 + kolikoJeManje(t, n)
				else
					kolikoJeManje(t, n)
		}
	}

	def kolikoJeManjeIter(l: List[Int], n: Int) : Int = {
		var c = 0
		for (el <- l) {
			if (el < n)
				c += 1
		}
		c
	}

	/*
		6. Napisati funkciju koja prihvata listu i vraca listu
		   svih prostih brojeva iz prvobitne liste.
	*/
	def prost(n: Int) : Boolean = {
		for (i <- 2 to n/2) {
			if (n % i == 0)
				return false
		}
		true
	}

	def vratiProste(l: List[Int]) : List[Int] =
		for (el <- l if prost(el)) yield el
	
	/*
		7. Napisati quicksort za sortiranje listi.
	*/
	def quicksort(l: List[Int]) : List[Int] = {
		l match {
			case Nil => Nil
			case (h: Int) :: Nil => List(h)
			case (p: Int) :: (t: List[Int]) =>
				quicksort(for (n <- t if n < p) yield n) ::: p :: quicksort(for (n <- t if n >= p) yield n)
		}
	}

}