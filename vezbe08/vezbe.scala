object Program {
	def main(args: Array[String]) = {
		// zad1()
		// zad2()
		// zad3()
		// zad4()
		zad6()
	}

	// 1. Napisati funkciju koja prima listu tacaka (tacka je par Double-ova)
  //  i jos jednu "centralnu" tacku (par Double-ova). Iz liste vraca sve tacke
  //  koja su od centralne tacke udaljene manje od neke prosledjene distance.
	def distanca(p1: Tuple2[Double, Double], p2: Tuple2[Double, Double]) : Double = {
		Math.sqrt(Math.pow(p1._1 - p2._2, 2) + Math.pow(p1._1 - p2._2, 2))
	}

	def vratiUnutrasnje(l: List[Tuple2[Double, Double]], c: Tuple2[Double, Double], d: Double): List[Tuple2[Double, Double]] = {
		l.filter(p => distanca(p, c) < d)
	}

	def zad1() = {
		var tacke: List[Tuple2[Double, Double]] = List(
			(10, 12),
			(4, 6),
			(3, 7),
			(5, 9)
		)

		var c: Tuple2[Double, Double] = (6, 6)

		var d = 1.5

		println(vratiUnutrasnje(tacke, c, d))
	}

	// 2. Napisati funkciju koja privhata listu Stringova, 
  //  razdvoji svaki po razmaku, te ih sve
  //  spoji zarezima. Koristiti map i reduce.
	def zameniRazmake(l: List[String]) : String = {
		l.flatMap(s => s.split(" ")).reduce((a, b) => a + "," + b)
	}

	def zad2() = {
		var a = List("aa a", "b bb")

		println(zameniRazmake(a))
	}

	// 3. Napisati funkciju koja prima niz brojeva, one koji su parni uveca za 1,
  //  a neparne kvadrida. Zatim, svaki od brojeva "okrece" (1234 -> 4321) i 
  //  na kraju izbacuje sve neparne.
	def krljanjeBrojki(a: Array[Int]): Array[Int] = {
		def okreni(a: Int) : Int = {
			a match {
				case b if b < 10 => b
				case b => (a % 10) * 10 + okreni(a / 10)
			}
		}

		a.map(d => if (d % 2 == 0) d + 1 else d * d).map(okreni).filter(d => d % 2 == 0)
	}

	def zad3() = {
		var a = Array(1,2,3,4,5,6)

		krljanjeBrojki(a).foreach { println }
	}

	// 4. Napisati funkciju koja ocekuje listu Int-ova i jos Int n. Za svaki od ostataka
  //  pri deljenju brojem n, funkcija treba da vrati koliko brojeva iz liste daje taj 
  //  ostatak. Potrebno je vratiti listu parova oblika:
   
  //    (ostatak, koliko brojeva iz liste daje ovaj ostatak)

  //  Primer: List(1, 3, 5, 6, 9), n = 3

  //  (0, 3),
  //  (1, 1),
  //  (2, 1)
	def prebroj(l: List[Int], n: Int) : List[Tuple2[Int, Int]] = {
		var ostaci = l.map(d => d % n)
		Range(0, n).map(d => (d, ostaci.count(_ == d))).toList
	}

	def zad4() = {
		var l = List(1, 3, 6, 9)
		println(prebroj(l, 3))
	}

	// 5. Definisati apstraktnu klasu Naselje. Naselje moze da bude Selo, Varosica ili Grad (case klase).
  //  Sva 3 tipa naselja mogu imati broj stanovnika (integer) i povrsinu (double). 
  //  Selo nosi informaciju o tome da li je "zbijeno" ili "razbijeno" (string). Grad 
  //  ima dodatni parametar koji kaze da li sadrzi gradski bazen ili ne (boolean).
	abstract class Naselje(val stan: Int, val povrs: Double)

	case class Selo(s: Int, p: Double, val zbijeno: Boolean) extends Naselje(s, p)
	case class Varosica(s: Int, p: Double) extends Naselje(s, p)
	case class Grad(s: Int, p: Double, val bazen: Boolean) extends Naselje(s, p)



	// 6. Napisati funkciju koja iz liste Naselja izdvaja sva razbijena sela i sve
	//    gradove sa bazenima koji imaju vise od 150 000 stanovnika.
	def izdvoji(l: List[Naselje]) : List[Naselje] = {
		l.filter(n =>
			n match {
				case Selo(_, _, true) => true
				case Grad(s, _, true) => s > 150000
				case _ => false
			}
		)
	}

	def zad6() = {
		var l = List(
			new Selo(1000, 100, false),
			new Selo(1000, 10, true),
			new Varosica(5000, 50),
			new Grad(30000, 200, true),
			new Grad(150001, 200, false),
			new Grad(150001, 200, true)
		)

		println(izdvoji(l))
	}
}