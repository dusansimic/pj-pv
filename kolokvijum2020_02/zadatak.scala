object Program {
	def main(args: Array[String]) = {
		// zad1_a()
		// zad1_b()
		// zad1_c()
		// zad1_d()
		// zad2_a()
		// zad2_b()
		// zad2_c()
		zad3_b()
	}

	// Zadatak 1 grupa a
	def okreniIPomnozi(l: List[Tuple2[Int, Int]]) : List[Tuple2[Int, Int]] =
		l.map{ case (x, y) => (y * 10, x) }
	
	def izbaciPrviManji(l: List[Tuple2[Int, Int]]) : List[Tuple2[Int, Int]] =
		okreniIPomnozi(l).filter{ case (a, b) => a >= b }

	def zad1_a() = {
		var l = List(
			(1, 2),
			(40, 3),
			(5, 6),
			(80, 7)
		)

		println(izbaciPrviManji(l))
	}

	// Zadatak 1 grupa b
	def obrni(l: List[String]) : List[String] =
		l.map(s => if (s.count(_.isDigit) >= 2) s.reverse else s)

	def spoji(l: List[String]) : String =
		obrni(l).reduce((acc, s) => acc + "-" + s)

	def zad1_b() = {
		var l = List("aa2a", "b2c3c5", "c1cc")

		println(spoji(l))
	}

	// Zadatak 1 grupa c
	def deliIliAps(l: List[Int]) : List[Int] = l.map(n => if (n >= 0) n / 2 else -n)

	// Ovo nije bas najjasnije kako treba da se uradi
	def sumiraj(l: List[List[Int]]) : Int = l.map(deliIliAps).map(_.sum).sum

	def zad1_c() = {
		var l = List(
			List(1, 2, 3),
			List(-1, 0, 1),
			List(-3, -2, -1)
		)

		println(sumiraj(l))
	}

	// Zadatak 1 grupa d
	def prosek(l: List[Double]) : Double = l.reduce((acc, n) => acc + n) / l.length
	
	def izbaciVanOpsega(l: List[List[Double]]) : List[Double] =
		l.map(prosek).filter(n => n >= 100.0 && n <= 200.0)

	def zad1_d() = {
		var l = List(
			List(50.0, 100.0, 200.0),
			List(1.0, 2.0, 3.0)
		)

		println(izbaciVanOpsega(l))
	}

	// Zadatak 2 grupa a
	abstract class Artikal(val sifra: String, val teznia: Double, val cena: Double)

	case class Jaja(s: String, t: Double, c: Double, val komada: Int) extends Artikal(s, t, c)
	case class Mleko(s: String, t: Double, c: Double) extends Artikal(s, t, c)
	case class Hleb(s: String, t: Double, c: Double, val beskvasni: Boolean) extends Artikal(s, t, c)

	def vrati_a(l: List[Artikal]): Double = l.filter{
		case Jaja(_, _, c, k) => (c < 150 && k >= 10) || (c > 200)
		case Hleb(_, t, _, b) => b && t > 0.5
		case _ => false
	}.map(_.cena).sum

	def zad2_a() = {
		var l = List(
			new Jaja("235670", 0.02, 130, 76),
			new Jaja("890256", 0.025, 160, 20),
			new Jaja("690578", 0.02, 260, 10),
			new Mleko("890456", 0.5, 200),
			new Hleb("460798", 0.4, 50, true),
			new Hleb("578934", 0.6, 50, false),
			new Hleb("650930", 0.55, 50, true),
		)

		println(vrati_a(l))
	}

	// Zadatak 2 grupa b
	abstract class Pice(val ime: String, val cena: Double, val kolicina: Double)

	case class Kafa(i: String, c: Double, k: Double, val cokoladica: Boolean) extends Pice(i, c, k)
	case class Sok(i: String, c: Double, k: Double) extends Pice(i, c, k)
	case class Pivo(i: String, c: Double, k: Double, val jacina: Double) extends Pice(i, c, k)

	def avg(l: List[Double]) : Double = l.sum / l.length

	def vrati_b(l: List[Pice]): Double = avg(l.filter{
		case Kafa(_, c, _, co) => c > 150 && !co
		case Pivo(_, _, k, j) => j > 6.0 && k > 0.33
		case Sok(_, _, k) => k < 0.3
	}.map(_.cena))

	def zad2_b() = {
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

		println(vrati_b(l))
	}

	// Zadatak 2 grupa c
	abstract class Teren(val sirina: Double, val duzina: Double, val cena: Double)

	case class FudbalskiTeren(s: Double, d: Double, c: Double, val mali: Boolean) extends Teren(s, d, c)
	case class KosarkaskiTeren(s: Double, d: Double, c: Double) extends Teren(s, d, c)
	case class OdbojkaskiTeren(s: Double, d: Double, c: Double, val visina: Double) extends Teren(s, d, c)

	def vrati_c(l: List[Teren]) : Double = avg(l.filter{
		case FudbalskiTeren(_, _, c, m) => !m && c <= 2000.0
		case OdbojkaskiTeren(_, _, c, _) => c > 500.0
		case _: KosarkaskiTeren => true
	}.map(_.cena))

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

	// Zadatak 3 gruba a
	// TODO

	// Zadatak 3 grupa b
	def tokenizacija(s: String) : Array[String] = s.toLowerCase.split(" ")

	def glavna(p: List[Tuple2[Int, String]]) = p
		.flatMap{
			case (id, tekst) => tokenizacija(tekst).map(w => (w, id))
		}
		.groupBy(_._1)
		.map{
			case (w, p) => (w, p.map(_._2).toList)
		}
		.toList

	def zad3_b() = {
		var p = List(
			(1, "Ana voli milovana"),
			(2, "Milovana voli ana"),
			(3, "Milovana boli kara")
		)

		println(glavna(p))
	}
}