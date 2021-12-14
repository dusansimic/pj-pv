object Program {
	def main(args: Array[String]) = {
		// zad2()
		// zad3()
		zad5()
		// zad6()
	}

	// 1. Definisati apstraktnu klasu Skola sa val poljima ime 
	//  i broj ucenika. Definisati case klase OsnovnaSkola i
	//  SrednjaSkola koje nasledjuju klasu Skola, primaju potrebne
	//  parametre i prosledjuju ih konstruktoru nadklase.
	abstract class Skola(val ime: String, val brojUcenika: Int)

	case class OsnovnaSkola(i: String, br: Int) extends Skola(i, br)
	case class SrednjaSkola(i: String, br: Int) extends Skola(i, br)

	// 2. Napisati funkciju koja prima listu skola, 
  //  te za svaku skolu, po tipu, ispise poruku
  //  "Osnovna/Srednja skola (ime) ima (broj) ucenika."
	def ispisiSkole(l: List[Skola]) : Unit = {
		l match {
			case Nil => return
			case h :: t => {
				h match {
					case OsnovnaSkola(i, br) => println(s"Osnovna skola ${i} ima ${br} ucenika.")
					case SrednjaSkola(i, br) => println(s"Srednja skola ${i} ima ${br} ucenika.")
				}
				ispisiSkole(t)
			}
		}
	}

	def zad2() = {
		var l: List[Skola] = List(
			new OsnovnaSkola("Djura Danicic", 400),
			new OsnovnaSkola("Djordje Natosevic", 300),
			new SrednjaSkola("Jovan Jovanovic Zmaj", 700),
			new OsnovnaSkola("Milos Crnjanski", 250),
		)

		ispisiSkole(l)
	}

	// 3. Napisati funkciju koja ce iz liste skola izbaciti sve
  //  skole koje imaju manje od 300 ucenika, koristeci funkciju
  //  filter.
	def izbaciManje(l: List[Skola]) : List[Skola] = {
		l.filter((s: Skola) => s.brojUcenika >= 300)
	}

	def zad3() = {
		var l: List[Skola] = List(
			new OsnovnaSkola("Djura Danicic", 400),
			new OsnovnaSkola("Djordje Natosevic", 300),
			new SrednjaSkola("Jovan Jovanovic Zmaj", 700),
			new OsnovnaSkola("Milos Crnjanski", 250),
		)

		println(izbaciManje(l))
	}

	// 4. Definisati apstraktnu klasu NebeskoTelo sa val poljima
	//  ime i precnik (u km). Nakon toga definisati case klase Planeta
	//  i Satelit koji nasledjuju apstraktnu klasu NebeskoTelo. 
	//  Planeta takodje ima polje "imaVodu", a Satelit ima polje
	//  "kruziOko" (String) koje sadrzi ime planete oko koje kruzi.
	abstract class NebeskoTelo(val ime: String, val precnik: Int)

	case class Planeta(i: String, p: Int, val imaVodu: Boolean) extends NebeskoTelo(i, p)
	case class Satelit(i: String, p: Int, val kruziOko: Planeta) extends NebeskoTelo(i, p)

	// 5. Napisati funkciju koja pomocu filter funkcije vrati samo one
	//    Satelite koji se nalaze u orbiti oko planete cije ime se prosledi
	//    kao parametar.
	def onlySateliti(n: NebeskoTelo) = n.isInstanceOf[Satelit]

	def onlyKruziOko(i: String) : (NebeskoTelo) => Boolean =
		(n: NebeskoTelo) => n.asInstanceOf[Satelit].kruziOko.ime == i
	
	def toSatelit(n: NebeskoTelo) : Satelit = n.asInstanceOf[Satelit]

	def vratiSatelite(l: List[NebeskoTelo], planeta: String) : List[Satelit] = {
		l.filter(onlySateliti).filter(onlyKruziOko(planeta)).map(toSatelit)
	}

	def zad5() = {
		var jupiter: Planeta = new Planeta("Jupiter", 1000, false)
		var europa: Satelit = new Satelit("Europa", 20, jupiter)
		var zemlja: Planeta = new Planeta("Zemlja", 200, true)
		var mesec: Satelit = new Satelit("Mesec", 10, zemlja)

		var l: List[NebeskoTelo] = List(
			jupiter, europa, zemlja, mesec
		)

		println(vratiSatelite(l, "Zemlja"))
	}

	// 6. Napisati funkciju koja vraca sve Planete koje imaju vodu i imaju 
	//    precnik veci od 5500 km. Takodje, treba da vrati sve satelite sa
	//    precnikom manjim od 1000 km.
	def vratiPlaneteISatelite(l: List[NebeskoTelo]) : List[NebeskoTelo] = {
		l match {
			case Nil => Nil
			case h :: t => {
				h match {
					case Planeta(_, p, v) =>
						if (p > 5500 && v)
							h :: vratiPlaneteISatelite(t)
						else
							vratiPlaneteISatelite(t)
					case Satelit(_, p, _) =>
						if (p < 1000)
							h :: vratiPlaneteISatelite(t)
						else
							vratiPlaneteISatelite(t)
				}
			}
		}
	}

	def zad6() = {
		var jupiter: Planeta = new Planeta("Jupiter", 100000, false)
		var europa: Satelit = new Satelit("Europa", 2000, jupiter)
		var zemlja: Planeta = new Planeta("Zemlja", 5600, true)
		var mesec: Satelit = new Satelit("Mesec", 900, zemlja)

		var l: List[NebeskoTelo] = List(
			jupiter, europa, zemlja, mesec
		)

		println(vratiPlaneteISatelite(l))
	}
}
