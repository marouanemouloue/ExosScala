package fr.mipn.pmr03

// plein d'objets !

object Bidule :
  private val secret = "123admin"
  def apply(s: String, n: Int) = new Bidule(s,n)
  def apply(s: String) = new Bidule(s, 0)

class Bidule(s: String, n: Int) :
  def saluer() : Unit = println(s"Salut je suis Bidule(${s}, ${n}) et je connais ton secret : ${Bidule.secret}")

trait Chanteur :
    def chanter() = println("lalalala")

trait Mangeur :
    def manger() = println("je mange")

trait Deplacement :
    def deplacer(): Unit = ???

trait Marcheur extends Deplacement :
    override def deplacer(): Unit = marcher()
    def marcher() = println("Je marche") 

trait Nageur extends Deplacement :
    override def deplacer(): Unit = nager()
    def nager() = println("Je nage")

class Animal extends Chanteur, Mangeur, Deplacement

class AnimalTerrestre extends Animal with Marcheur
class AnimalMarin extends Animal with Nageur

trait UtilsNombres :
    def inttoordre(n: Int) : String = 
    n match
        case 1 => "premier"
        case 2 => "second"
        case n if n > 0 => n.toString + "me"

sealed trait ExpAlg
case class Add( e1 : ExpAlg, e2 : ExpAlg) extends ExpAlg
case class Mult( e1 : ExpAlg, e2 : ExpAlg) extends ExpAlg
case class Entier(n : Int) extends ExpAlg
case object Zero extends ExpAlg


trait ExpAlgUtils :
    def prettyPrinter(e: ExpAlg) : String = e match 
        case Add(e1, e2) => s"(${prettyPrinter(e1)} + ${prettyPrinter(e2)})"
        case Mult(e1, e2) => s"(${prettyPrinter(e1)} + ${prettyPrinter(e2)})"
        case Entier(n) => s"$n"
  
object Menteur :
    def apply(n: Int, s: String) = new Menteur(n, s)
    def unapply(m: Menteur) : Option[(Int, String)] = Some(m.n, "quarante deux")

class Menteur(val n: Int, val s: String)


@main def pmr03: Unit =
  println("Hello PMR 03!")
  Bidule("salut").saluer()
  val a = new AnimalTerrestre
  a.manger()
  a.chanter()
  a.deplacer()
  val m = Menteur(3, "trois")
  val Menteur(_, enlettres) = m
  println(enlettres)
  m match 
    case Menteur(_,"quarante deux") => println("menteur !")
    case Menteur(_,"trois") => println("ah bon ?")
  // implicit avec using et given
  def additionner(n: Int, m: Int)(using log: String => Unit) =
    log(s"j'additionnne ${n} et ${m}")
    n + m
  given defaultlogger: (String => Unit) = (s: String) => println(s"log: ${s}")

  additionner(3, additionner(5,2))

  def multiplier(n: Int, m: Int)(using log: String => Unit) =
    log(s"je multiplie ${n} et ${m}")
    n * m

  def calculer()(using String => Unit) = multiplier(4,additionner(3,5))

  calculer()
  println("Sayonara")