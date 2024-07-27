object InventorySystem extends App {

  case class Product(id: Int, name: String, quantity: Int, price: Double)

  val inventory1: Map[Int, Product] = Map(
    101 -> Product(101, "Product A", 10, 15.00),
    102 -> Product(102, "Product B", 5, 20.00),
    103 -> Product(103, "Product C", 20, 5.00)
  )

  val inventory2: Map[Int, Product] = Map(
    102 -> Product(102, "Product B", 10, 22.00),
    104 -> Product(104, "Product D", 15, 10.00)
  )

  def retrieveProductNames(inventory: Map[Int, Product]): List[String] = {
    inventory.values.map(_.name).toList
  }

  println(s"Product names in inventory1: ${retrieveProductNames(inventory1)}")

  def calculateTotalValue(inventory: Map[Int, Product]): Double = {
    inventory.values.map(product => product.quantity * product.price).sum
  }

  println(s"Total value of inventory1: ${calculateTotalValue(inventory1)}")

  def isInventoryEmpty(inventory: Map[Int, Product]): Boolean = {
    inventory.isEmpty
  }

  println(s"Is inventory1 empty? ${isInventoryEmpty(inventory1)}")

  def mergeInventories(inventory1: Map[Int, Product], inventory2: Map[Int, Product]): Map[Int, Product] = {
    (inventory1.keys ++ inventory2.keys).map { key =>
      val product1 = inventory1.get(key)
      val product2 = inventory2.get(key)

      val mergedProduct = (product1, product2) match {
        case (Some(p1), Some(p2)) =>
          Product(
            id = p1.id,
            name = p1.name,
            quantity = p1.quantity + p2.quantity,
            price = Math.max(p1.price, p2.price)
          )
        case (Some(p1), None) => p1
        case (None, Some(p2)) => p2
        case _ => throw new Exception("Unexpected case")
      }

      key -> mergedProduct
    }.toMap
  }

  val mergedInventory = mergeInventories(inventory1, inventory2)
  println(s"Merged inventory: $mergedInventory")

  def checkAndPrintProductDetails(inventory: Map[Int, Product], productId: Int): Unit = {
    inventory.get(productId) match {
      case Some(product) => println(s"Product details for ID $productId: $product")
      case None => println(s"Product with ID $productId not found.")
    }
  }

  checkAndPrintProductDetails(inventory1, 102)
}
