object Program {
	def main(args: Array[String]) = {
		// zad1_a()
		// zad1_b()
		// zad1_c()
		// zad1_d()
		// zad2_a()
		// zad2_b()
		// zad2_c()
		// zad3_a()
		// zad3_b()
	}

	// Zadatak 1 grupa a
	def izbaciDrugiNeg(l: List[Tuple2[Int, Int]]) = l.filter(_._2 >= 0)

	def vozdigni(l: List[Tuple2[Int, Int]]) =
		izbaciDrugiNeg(l).map{
			case (x, y) => Math.pow(x, y)
		}.reduce((acc, n) => acc + n)

	def zad1_a() = {
		var l = List(
			(2, 2),
			(4, -5),
			(6, 3),
			(9, -1)
		)

		println(vozdigni(l))
	}

	// Zadatak 1 grupa b
	def prosek(l: List[List[String]]) : List[Double] =
		l.map(ls => ls.map(_.length).sum * 1.0 / ls.size)

	def prosekProseka(l: List[List[String]]) : Double = {
		var proseci = prosek(l)
		var p = proseci.sum / l.size
		proseci.filter(_ <= p).sum
	}

	def zad1_b() = {
		var l = List(
			List("ana", "voli", "milovana"),
			List("milovana", "voli", "ana"),
			List("milovana", "boli", "kara")
		)

		println(prosekProseka(l))
	}

	// Zadatak 1 grupa c
	def okreniIPomnozi(l: List[Tuple2[Int, Int]]) : List[Tuple2[Int, Int]] =
		l.map{ case (x, y) => (y * 10, x) }
	
	def izbaciPrviManji(l: List[Tuple2[Int, Int]]) : List[Tuple2[Int, Int]] =
		okreniIPomnozi(l).filter{ case (a, b) => a >= b }

	def zad1_c() = {
		var l = List(
			(1, 2),
			(40, 3),
			(5, 6),
			(80, 7)
		)

		println(izbaciPrviManji(l))
	}

	// Zadatak 1 grupa d
	def obrni(l: List[String]) : List[String] =
		l.map(s => if (s.count(_.isDigit) >= 2) s.reverse else s)

	def spoji(l: List[String]) : String =
		obrni(l).reduce((acc, s) => acc + "-" + s)

	def zad1_d() = {
		var l = List("aa2a", "b2c3c5", "c1cc")

		println(spoji(l))
	}

	// Zadatak 2 grupa a
	abstract class Pice(val ime: String, val cena: Double, val kolicina: Double)

	case class Kafa(i: String, c: Double, k: Double, val cokoladica: Boolean) extends Pice(i, c, k)
	case class Sok(i: String, c: Double, k: Double) extends Pice(i, c, k)
	case class Pivo(i: String, c: Double, k: Double, val jacina: Double) extends Pice(i, c, k)

	def avg(l: List[Double]) : Double = l.sum / l.length

	def vrati_a(l: List[Pice]): Double = avg(l.filter{
		case Kafa(_, c, _, co) => c > 150 && !co
		case Pivo(_, _, k, j) => j > 6.0 && k > 0.33
		case Sok(_, _, k) => k < 0.3
	}.map(_.cena))

	def zad2_a() = {
		var l = List(
			new Kafa("", 130, 0.2, false),
			new Kafa("", 160, 0.2, true),
			new Kafa("", 155, 0.2, false),
			new Sok("", 160, 0.5),
			new Sok("", 120, 0.25),
			new Pivo("", 180, 0.5, 5.0),
			new Pivo("", 190, 0.33, 6.5),
			new Pivo("", 200, 0.5, 9.0)
		)

		println(vrati_a(l))
	}

	// Zadatak 2 grupa b
	abstract class Sat(val proizvodjac: String, val cena: Double)

	case class RucniSat(p: String, c: Double, val kais: String) extends Sat(p, c)
	case class ZidniSat(p: String, c: Double) extends Sat(p, c)
	case class PametniSat(p: String, c: Double, val baterija: Double) extends Sat(p, c)

	def vrati_b(l: List[Sat]) : Double = l.collect{
		case RucniSat(_, c, k) if k == "metal" => c
		case ZidniSat(_, c) if true => c
		case PametniSat(_, c, b) if b < 8.0 => c
	}.sum

	def zad2_b() = {
		var l = List(
			new RucniSat("Casio", 50.0, "koza"),    // false
			new RucniSat("MVMT", 60.0, "metal"),    // true
			new RucniSat("Rolex", 1000.0, "metal"), // true
			new ZidniSat("Kinez", 2.0),             // true
			new PametniSat("Apple", 400.0, 36.0),   // false
			new PametniSat("Kinez", 20.0, 5.0)      // true
		)

		println(vrati_b(l))
	}

	// Zadatak 2 grupa c
	abstract class Teren(val sirina: Double, val duzina: Double, val cena: Double)

	case class FudbalskiTeren(s: Double, d: Double, c: Double, val mali: Boolean) extends Teren(s, d, c)
	case class KosarkaskiTeren(s: Double, d: Double, c: Double) extends Teren(s, d, c)
	case class OdbojkaskiTeren(s: Double, d: Double, c: Double, val visina: Double) extends Teren(s, d, c)

	def vrati_c(l: List[Teren]) : Double = avg(l.collect{
		case FudbalskiTeren(_, _, c, m) if !m && c <= 2000.0 => c
		case OdbojkaskiTeren(_, _, c, _) if c > 500.0 => c
		case KosarkaskiTeren(_, _, c) => c
	})

	def zad2_c() = {
		var l = List(
			new FudbalskiTeren(150, 200, 1500.0, false),
			new FudbalskiTeren(150, 200, 2500.0, true),
			new FudbalskiTeren(150, 200, 2000.0, false),
			new KosarkaskiTeren(50, 75, 1000.0),
			new OdbojkaskiTeren(50, 75, 1000.0, 2.0),
			new OdbojkaskiTeren(50, 75, 250.0, 2.0),
		)

		println(vrati_c(l))
	}

	// Zadatak 3 grupa a
	// Urađeno zahvaljujući rešenju Aleksandra Rončevića
	type Tacka3 = Tuple3[Double, Double, Double]

	def kNN(l: List[Tuple2[Tacka3, Int]], t: Tacka3, k: Int) : Int = l.size match {
		case len if len < k => -1
		case _ => kNN_(l, t, k)
	}

	def kNN_(l: List[Tuple2[Tacka3, Int]], t: Tacka3, k: Int) : Int = {
		var distance = l.map(p => (dist3(p._1, t), p._2))

		distance = qs(distance)

		distance
			.take(k)                     // Uzima se k najbližih tačaka
			.groupBy(_._2)               // Tačke se grupišu po klasterima kojima pripadaju
			.map(p => (p._1, p._2.size)) // Pošto onda za vrednost HashMap-e imamo listu distanci
			                             // (tačaka), prebrojimo koliko izdvojenih tačaka ima u kom
			                             // klasteru
			.maxBy(_._2)                 // Uzima se maksimalni par (klaster, broj tačaka) po broju
			                             // tačaka
			._1                          // Vraća se samo broj klastera
	}

	def dist3(t1: Tacka3, t2: Tacka3) : Double =
		Math.sqrt(
			Math.pow(t1._1 - t2._1, 2) +
			Math.pow(t1._2 - t2._2, 2) +
			Math.pow(t1._3 - t2._3, 2)
		)

	def qs(l: List[Tuple2[Double, Int]]) : List[Tuple2[Double, Int]] = l match {
		case Nil => Nil
		case (h: Tuple2[Double, Int]) :: Nil => List(h)
		case (p: Tuple2[Double, Int]) :: (t: List[Tuple2[Double, Int]]) =>
			qs(for (n <- t if n._1 < p._1) yield n) ::: p :: qs(for (n <- t if n._1 >= p._1) yield n)
	}

	def zad3_a() = {
		var l = List(
			((3.0,4.0,5.0),1),
			((2.0,4.0,3.0),1),
			((1.0,4.0,2.0),2),
			((6.0,4.0,5.0),2),
			((1.2,4.0,2.3),2),
			((4.5,3.0,5.0),1),
		)
		var t = (1.0, 1.0, 1.0)

		println(kNN(l, t, 3))
	}

	// Zadatak 3 grupa b
	type Tacka = Tuple2[Double, Double]

	def kMeans(l: List[Tacka], k: Int) : List[Tuple2[Tacka, Int]] =
		l.length match {
			case len if len < k => Nil
			case _ => km_(l, l.take(k), List())
		}
	
	def mind(p: Tacka, pl: List[Tacka]) : Int = {
		def dist(p1: Tacka, p2: Tacka) : Double =
			Math.sqrt(Math.pow(p1._1 - p2._1, 2) + Math.pow(p1._2 - p2._2, 2))

		var pl2 = pl.indexOf(pl.reduce((acc, pp) => if (dist(p, pp) < dist(p, acc)) pp else acc))
		pl2
	}

	def center(g: List[Tacka]) : Tacka = {
		var p = g.reduce((acc, p) => (acc._1 + p._1, acc._2 + p._2))
		(p._1 / g.size, p._2 / g.size)
	}
	
	def km_(l: List[Tacka], c: List[Tacka], cp: List[Tacka]) : List[Tuple2[Tacka, Int]] = {
		var groups = l.groupBy(p => mind(p, c))

		if (c == cp)
			groups.flatMap{
				case (i, g) => g.map((_, i))
			}.toList
		else {
			var nc = Array.ofDim[Tacka](c.size)
			for ((i, g) <- groups) {
				nc(i) = center(g)
			}
			km_(l, nc.toList, c)
		}
	}

	def zad3_b() = {
		// var l: List[Tacka] = List()
		// for (i <- 1 to 100) {
		// 	l = l :+ (Math.random(), Math.random())
		// }
		var l: List[Tacka] = List(
			(5, 5),
			(6, 6),
			(8, 1),
			(4, 2),
			(8, 3),
			(9, 5),
			(6, 7)
		)

		println(kMeans(l, 3))
	}
}
