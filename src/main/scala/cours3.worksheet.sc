1 + 1
Option("Salut")
Option("Salut").map(_.length)
val x: Option[String] = None 
x.map(_.length)
val y:Either[Int,String] = Left(3)
y.map(_ + 2)
import scala.util.Try
Try(0/1).map(100/_)
Try(1/0).toOption
def from(n: Int): LazyList[Int] = n #:: from(n + 1)
val z = from(2)
z.take(5).toList
def sieve(s: LazyList[Int]): LazyList[Int] =
  s.head #:: sieve(s.tail.filter(_ % s.head != 0))
sieve(from(2)).take(10).toList
def inttoordre(n: Int) : String = 
    n match
        case 1 => "premier"
        case 2 => "second"
        case n if n > 0 => n.toString + "ème"

inttoordre(10)

def identite[A](x: A) = x match 
    case x: Int => x + 1
    case _: A  => x 



identite(1)
identite("je suis une string")

def cestbienlestypesunion(x: Int | String | List[Double]) = 
    x match
        case x : Int => List(x/ 3.0, x/4.0)
        case _  => x
    
cestbienlestypesunion(List(3.0,4.2))
cestbienlestypesunion(2)

val montuple = (x, "salut", List(3,4))

val (a,b,c) = montuple
val aa = montuple._1
val bb = montuple._2

val s"${prenom} ${nom}" = "Pierre Comte de la Fontaine"

prenom 

case class Person(prenom:String, nom:String)

val unepersonne = Person("Steeve","Banner")
val Person(prenom2, nom2) = unepersonne


trait ExpAlg
case class Add( e1 : ExpAlg, e2 : ExpAlg) extends ExpAlg
case class Mult( e1 : ExpAlg, e2 : ExpAlg) extends ExpAlg
case class Entier(n : Int) extends ExpAlg

/* 1 + 2 × (3 + 4)*/
Add(Entier(1), Mult(Entier(2), Add(Entier(3), Entier(4))))

def prettyPrinter(e: ExpAlg) : String = 
    e match 
        case Add(e1, e2) => s"(${prettyPrinter(e1)} + ${prettyPrinter(e2)})"
        case Entier(n) => s"$n"
    
 prettyPrinter(Add(Entier(3), Add(Entier(2), Entier(5))))       