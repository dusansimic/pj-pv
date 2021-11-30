import scala.io.StdIn._

object Program {

	def main(args: Array[String]) : Unit = {
	}

	/*
		1. Napisati funkciju tipKaraktera koja prima Char, odredjuje da li je
		   on veliko, malo ili nije slovo uopste, i vraca poruku kao String.
	*/
	def tipKaraktera(c: Char) : String =
		if (c >= 'a' && c <= 'z')
			"malo"
		else if (c >= 'A' && c <= 'Z')
			"veliko"
		else
			"nije slovo"

	/*
		2. Napisati funkciju pomnoziSa koja prima Int i niz Int-ova,
		   a kao rezultat vraca prvi broj pomnozen sa svim brojevima
		   niza.
	*/
	def pomnoziSa(a: Int, l: Array[Int]) : Int = {
		var b = a
		for (n <- l) {
			b = b * n
		}
		b
	}

	/*
		3. Napisati funkciju koja kvadrira sve brojeve nekog niza. (Koristiti
		   yield).
	*/
	def kvadriraj(arr: Array[Int]) : Array[Int] = {
		for (a <- arr) yield {
			a * a
		}
	}

	/*
		4. Napisati funkciju koja prima Char i Int. U while petlji ispisuje
		   karakter c, n puta.
	*/
	def ispisi(c: Char, n: Int) = {
		for (_ <- 1 to n)
			print(c)
		println()
	}

	/*
		5. Napisati funkciju kalkulator, koja prihvata 2 Int-a i Char
		   (+, -, *, /), i pomocu match-case utvrdjuje koju operaciju
		   treba da izvrsi. Za nepoznatu operaciju vratiti 0. Ucitati
		   podatke od korisnika.
	*/
	def kalkulator(a: Int, b: Int, o: Char) : Int = {
		o match {
			case '+' => a + b
			case '-' => a - b
			case '*' => a * b
			case '/' => a / b
			case _ => 0
		}
	}

	/*
		6. Napisati funkciju koja utvrdjuje da li je prosledjeni argument nekog
		   brojevnog tipa (Int, Float...) ili nije.
	*/
	def proveraTipa(p: Any) : Boolean = {
		p match {
			case n: Int => true
			case n: Float => true
			case n: Double => true
			case _ => false
		}
	}

	/*
		7. Napisati funkciju koja prima par Int-ova i broj n,
		   vraca novi par gde je prvi element para pomnozen sa n
		   a drugom elementu para je dodat broj n.
	*/
	def izmeniPar(p: (Int, Int), n: Int) : Tuple2[Int, Int] = (p._1 * n, p._2 + n)

	/*
		8. Napisati funkciju "spljosti" koja radi sa listama.

		   [1, 2, 2, 2, 3, 3, 4, 5, 5, 5] -> [1, 2, 3, 4, 5]
	*/
	def spljosti(l: List[Int]): List[Int] = {
		l match {
			case Nil => Nil
			case (x: Int) :: Nil => x :: Nil
			case (x: Int) :: (y: Int) :: (t: List[Int]) =>
				if (x == y)
					spljosti(x :: t)
				else
					x :: spljosti(y :: t)
		}
	}
}
