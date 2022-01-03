object Program {
	def main(args: Array[String]) = {
		// zad1()
		// zad2()
		// zad3()
	}


	// 1. Definisati apstraktnu klasu Stednja koja predstavlja stedni racun u banci. 
	//    Stednja moze da bude DinarskaStednja ili DeviznaStednja (case klase). Obe 
	//    stednje kao osobine imaju kamatnu stopu (double) i informaciju da li su 
	//    orocene ili neorocene (boolean). DeviznaStednja, za razliku od Dinarske, 
	//    takodje ima boolean koji predstavlja da li je u evrima ili nije. 
	abstract class Stednja(val stopa: Double, val orocena: Boolean)

	case class DinarskaStednja(s: Double, o: Boolean) extends Stednja(s, o)
	case class DeviznaStednja(s: Double, o: Boolean, val evri: Boolean) extends Stednja(s, o)

	// 2. Napisati funkciju koja iz liste Stednji vraca dinarske stednje koje
	//    koje imaju kamatnu stopu vecu od 2.
	def vratiDinaskeKamataVeca2(l: List[Stednja]) : List[Stednja] = {
		l.filter(s => s match {
			case DinarskaStednja(s, _) => s > 2
			case _ => false
		})
	}

	def zad1() = {
		var l: List[Stednja] = List(
			new DinarskaStednja(5, true),
			new DinarskaStednja(1, false),
			new DinarskaStednja(2, true),
			new DinarskaStednja(3, false),
			new DeviznaStednja(5, false, true),
			new DeviznaStednja(2, true, false)
		)

		println(vratiDinaskeKamataVeca2(l))
	}

	// 3. Napisati funkciju koja iz liste stednji vraca dinarske stednje koje su
	//    orocene i devizne stednje koje su u evrima.
	def vratiDinarskeOroceneDevizneEvri(l: List[Stednja]) : List[Stednja] = {
		l.filter(s => s match {
			case DinarskaStednja(_, o) => o
			case DeviznaStednja(_, _, e) => e
			case _ => false
		})
	}

	def zad2() = {
		var l: List[Stednja] = List(
			new DinarskaStednja(5, true),
			new DinarskaStednja(1, false),
			new DinarskaStednja(2, true),
			new DinarskaStednja(3, false),
			new DeviznaStednja(5, false, true),
			new DeviznaStednja(2, true, false)
		)

		println(vratiDinarskeOroceneDevizneEvri(l))
	}

	// 4. Napisati funkciju koja ocekuje listu stednji i racuna prosek kamatnih stopa
	//    stednji koje su orocene.
	def prosekOrocenihStopa(l: List[Stednja]) : Double = {
		var orocene = l.collect{
			case s if s.orocena => s.stopa
		}
		orocene.sum / orocene.length
	}

	def zad3() = {
		var l: List[Stednja] = List(
			new DinarskaStednja(5, true),
			new DinarskaStednja(1, false),
			new DinarskaStednja(2, true),
			new DinarskaStednja(3, false),
			new DeviznaStednja(5, false, true),
			new DeviznaStednja(3, true, false)
		)

		println(prosekOrocenihStopa(l))
	}
}
