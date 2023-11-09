package fr.mipn.main

sealed trait Message
case class OrderItem(quantity: Int, product: String, table: Int,message:String) extends Message
case class Invoice(table: Int) extends Message

def additions(prices: Map[String, Int], messages: List[Message]): List[(Int, Int, String)] = {
  var totalByTable = Map.empty[Int, Int]
  var result = List.empty[(Int, Int, String)]
  var mess = ""

  messages.foreach {
    case OrderItem(quantity, product, table,msgReceived) =>
      val price = prices.getOrElse(product, 0)
      val total = totalByTable.getOrElse(table, 0) + (quantity * price)
      totalByTable += (table -> total)
      mess = msgReceived
    case Invoice(table) =>
      val total = totalByTable.getOrElse(table, 0)
      result = result :+ (table, total, mess)
      totalByTable -= table
    
  }

  result
}

@main def hello: Unit =
  val prices = Map("café" -> 3, "thé" -> 12)

  val exemple = List(
    OrderItem(1, "café", 3,""),
    OrderItem(1, "thé", 3,""),
    OrderItem(1, "café", 2,""),
    Invoice(3),
    OrderItem(1, "café", 2,"Merci"),
    Invoice(2),
    OrderItem(2, "café", 3,""),
    Invoice(3)
  )

  val result = additions(prices, exemple)

  result.foreach {
    case (table, total, message) =>
      println(s"addition pour la $table, $total €, \n$message")
    
  }
