package fr.mipn.main

sealed trait Message
case class OrderItem(quantity: Int, product: String, table: Int) extends Message
case class Invoice(table: Int) extends Message
case class ThankYou() extends Message


def additions(prices: Map[String, Int], messages: List[Message]): List[(Int, Int)] = {
  var totalByTable = Map.empty[Int, Int]
  var result = List.empty[(Int, Int)]
  var mess = ""

  messages.foreach {
    case OrderItem(quantity, product, table) =>
      val price = prices.getOrElse(product, 0)
      val total = totalByTable.getOrElse(table, 0) + (quantity * price)
      totalByTable += (table -> total)
    case Invoice(table) =>
      val total = totalByTable.getOrElse(table, 0)
      result = result :+ (table, total)
      totalByTable -= table
    case ThankYou() =>
     mess= "Merci"
  }

  result
}
@main def hello: Unit =
  val prices = Map("café" -> 3, "thé" -> 12)

  val exemple = List(
    OrderItem(1, "café", 3),
    OrderItem(1, "thé", 3),
    OrderItem(1, "café", 2),
    Invoice(3),
    OrderItem(1, "café", 2),
    Invoice(2),
    ThankYou(),
    OrderItem(2, "café", 3),
    Invoice(3)
  )

  val result = additions(prices, exemple)

  result.foreach { case (table, total) =>
    println(s"addition pour la $table, $total €")
  }

