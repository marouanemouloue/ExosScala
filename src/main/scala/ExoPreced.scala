package fr.mipn.exo03


//Ex
def partionner(phrase:String):List[String]={
        var i:Int = 0
        var list: List[String] = Nil
        var breakLoop = false
        for(c <- phrase ){
            if(c.equals('-')&& !breakLoop){
                val a = i
                val s1= phrase.substring(0,a)
                val s2 = phrase.substring(a+1,phrase.length())
                list =  List(s1,s2)              
                breakLoop = true
            }
            i += 1
        }
        list
}
//exA
def checkLongueurs(mot: List[String], longueur: List[Int]): Boolean = {
  mot.forall { motElement =>
    longueur.contains(motElement.length)
  }
}
//exB
val list = ("Buenas Dia").replace(' ','_')
//exC
def prefixes(word: String): List[String] = {
  var result: List[String] = List("")
  for (i <- 1 to word.length) {
    val prefix = word.substring(0, i)
    result = prefix :: result
  }
  result.reverse
}
//ExC
def findWordInText(word: String, text: String): Option[String] = {
  val wordIndex = text.indexOf(word)
  if (wordIndex >= 0) Some(text.substring(wordIndex, wordIndex + word.length))
  else None
}
//Ex5
def calculateExpression(E: Set[Int], F: Set[Int], G: Set[Int], H: Set[Int]): Set[Int] = {
  val result: Set[Int] = (E.intersect(F)) union (G.diff(H))
  result
}


