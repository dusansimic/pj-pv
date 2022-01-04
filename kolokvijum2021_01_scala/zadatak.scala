object Program {
	def main(args: Array[String]) = {
		// zad1_a_exec()
		// zad1_b_exec()
		// zad1_c_exec()
		// zad1_d_exec()
		// zad2_a_exec()
		// zad2_b_exec()
		// zad2_c_exec()
		// zad3_a_exec()
		zad3_b_exec()
	}

	// Zadatak 1 grupa a
	def zad1_a(s: String) : String = {
		s.filter(c => c.isDigit)
	}

	def zad1_a_exec() = {
		println(zad1_a("a4bc123dfg"))
	}

	// Zadatak 1 grupa b
	def zad1_b(s: String) : Boolean = {
		var ss = s.toLowerCase().filter(c => c != ' ')
		ss == ss.reverse
	}

	def zad1_b_exec() = {
		println(zad1_b("Ana voli Milovana"))
		println(zad1_b("Ana ne voli Milovana"))
	}

	// Zadatak 1 grupa c

	def isPrime(n: Int) : Boolean =
		(n > 1) && !(2 to Math.sqrt(n).toInt).exists(x => n % x == 0)

	def zad1_c_prosti(l: List[Int]) : List[Int] = {
		l.filter(isPrime(_))
	}

	def zad1_c_ostatak(l: List[Int]) : List[Int] = {
		zad1_c_prosti(l).map(_ % l.length)
	}

	def zad1_c_exec() = {
		var l = List(2, 3, 5, 7, 11)
		println(zad1_c_ostatak(l))
	}

	// Zadatak 1 grupa d
	def zad1_d_pod(l: List[List[Int]]) : List[Int] = {
		l.map(ll => if (ll.length >= 5) ll.sum else ll.product)
	}

	def zad1_d_kvadriraj(l: List[List[Int]]) : Int = {
		zad1_d_pod(l).map(n => if (n % 3 == 0) n * n else n).sum
	}
	
	def zad1_d_exec() = {
		var l = List(
			List(1, 2, 3, 4),
			List(5, 6, 7, 8, 9),
			List(10, 11)
		)
		println(zad1_d_kvadriraj(l))
	}

	// Zadatak 2 grupa a
	abstract class KucniLjubimac(val ime: String, val hrana: Double, val godine: Int)

	case class Macka(i: String, h: Double, g: Int, val izlazi: Boolean) extends KucniLjubimac(i, h, g)
	case class Pas(i: String, h: Double, g: Int, val rasa: String) extends KucniLjubimac(i, h, g)

	def zad2_a(l: List[KucniLjubimac]) : List[KucniLjubimac] = {
		l.filter{
			case Macka(_, h, g, i) => (g < 5) || (!i && h < 0.2)
			case Pas(_, h, _, r) => r == "zlatni retriver" && h > 1.0
			case _ => false
		}
	}

	def zad2_a_exec() = {
		var l = List(
			new Macka("Marko", 0.3, 2, false),
			new Macka("Garfild", 1, 6, true),
			new Macka("Zoran", 0.1, 5, false),
			new Pas("Kosta", 0.3, 4, "snaucer"),
			new Pas("Rajli", 1.1, 5, "zlatni retriver")
		)
		println(zad2_a(l))
	}

	// Zadatak 2 grupa b
	abstract class Stednja(val stopa: Double, val orocena: Boolean)

	case class DinarskaStednja(s: Double, o: Boolean) extends Stednja(s, o)
	case class DeviznaStednja(s: Double, o: Boolean, val evri: Boolean) extends Stednja(s, o)

	def zad2_b(l: List[Stednja]) : List[Stednja] = {
		l.filter{
			case DinarskaStednja(s, _) => s > 2.9
			case DeviznaStednja(s, o, e) => s > 1.0 || (!e && o)
			case _ => false
		}
	}

	def zad2_b_exec() = {
		var l = List(
			new DinarskaStednja(5, true),
			new DinarskaStednja(1, false),
			new DinarskaStednja(2, true),
			new DinarskaStednja(3, false),
			new DeviznaStednja(5, false, true),
			new DeviznaStednja(1, true, false)
		)

		println(zad2_b(l))
	}

	// Zadatak 2 grupa c
	abstract class StambeniObjekat(val kvadratura: Double, val kapacitet: Int)

	case class SeoskaKuca(kv: Double, ka: Int, val dvoriste: Double) extends StambeniObjekat(kv, ka)
	case class GradskaKuca(kv: Double, ka: Int) extends StambeniObjekat(kv, ka)
	case class Zgrada(kv: Double, ka: Int, val spratovi: Int) extends StambeniObjekat(kv, ka)

	def zad2_c(l: List[StambeniObjekat]) : Int = {
		l.collect{
			case SeoskaKuca(_, k, d) if d >= 5.0 => k
			case GradskaKuca(_, k) => k
			case Zgrada(_, k, s) if s >= 7 => k
		}.sum
	}

	def zad2_c_exec() = {
		var l = List(
			new SeoskaKuca(40.0, 2, 4.0),
			new SeoskaKuca(60.0, 4, 7.0),
			new GradskaKuca(30.0, 2),
			new GradskaKuca(50.0, 4),
			new Zgrada(70.0, 5, 10),
			new Zgrada(40.0, 3, 5)
		)

		println(zad2_c(l))
	}

	// Zadatak 3 grupa a
	def jeKvadratna(m: Array[Array[Int]]) : Boolean = {
		var x = m.length
		var y = m.reduce{(acc, e) => acc ++ e}.length
		var yEq = m.forall(_.length == m.head.length)

		x * x == y && yEq
	}

	def det_trivijalno(m: Array[Array[Int]]) : Int =
		m(0)(0) * m(1)(1) - m(0)(1) * m(1)(0)
	
	def minor(m: Array[Array[Int]], i: Int) : Array[Array[Int]] =
		m.map(a => a.take(i) ++ a.drop(i + 1))
	
	def det_obicno(m: Array[Array[Int]]) : Int =
		(0 to m.length - 1).map(i => Math.pow(-1, i) * m.head(i) * det_racun(minor(m.tail, i))).sum.toInt
	
	def det_racun(m: Array[Array[Int]]) : Int =
		m.length match {
			case l if l > 2 => det_obicno(m)
			case _ => det_trivijalno(m)
		}
	
	def det(m: Array[Array[Int]]) : Int =
		if (jeKvadratna(m))
			det_racun(m)
		else
			0

	def zad3_a_exec() = {
		var m = Array(
			Array(-2, 2, -3),
			Array(-1, 1, 3),
			Array(2, 0, -1)
		)

		println(det(m))
	}

	def concat(l: IndexedSeq[List[List[Int]]]) : List[List[Int]] = l.reduceOption(_ concat _).getOrElse(List())

	// Zadatak 3 grupa b
	def proveri(l: List[Int], k: Int) : Boolean =
		(0 to l.length - 1).forall(i => l(i) != k && (Math.abs(i - l.length) != Math.abs(l(i) - k)))
	
	def pronadji_(n: Int, l: List[Int]) : List[List[Int]] = {
		l.length match {
			case i if i == n => List(l)
			case _ => concat(
				(0 to n - 1).filter(proveri(l, _)).map(k => pronadji_(n, l ::: List(k)))
			)
		}
	}

	def pronadji(n: Int) : List[List[Int]] = pronadji_(n, Nil)

	def zad3_b_exec() = {
		println(pronadji(8).length)
	}
}