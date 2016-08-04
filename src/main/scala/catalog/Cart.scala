package catalog

trait Cart {
  def quantity(i: Item): Quantity
  def -- (items: Traversable[(Item, Quantity)]): Cart
  def items: Set[(Item, Quantity)]
}

object Cart {
  def apply(cartItems: Set[(Item, Quantity)]): Cart = new CartImpl(cartItems)

  private def toQuantMap(itemSet: Traversable[(Item, Quantity)]) =
    itemSet map {
      case (item, quant) => (item.id, quant)
    } toMap

  private class CartImpl(cartItems: Set[(Item, Quantity)]) extends Cart with Equals {

    private val itemIdQuantMap = toQuantMap(cartItems)

    def quantity(item: Item) = itemIdQuantMap.getOrElse(item.id, 0)

    def items: Set[(Item, Quantity)] = cartItems

    def -- (minusItems: Traversable[(Item, Quantity)]) = {
      val minusQuantMap = toQuantMap(minusItems)
      val reducedCartItems = for {
        (item, quant) <- cartItems
        reducedQuant = quant - minusQuantMap.getOrElse(item.id, 0)
        if (reducedQuant > 0)
      } yield (item, reducedQuant)
      Cart(reducedCartItems)
    }

    def canEqual(other: Any) = {
      other.isInstanceOf[CartImpl]
    }

    override def equals(other: Any) = {
      other match {
        case that: CartImpl => that.canEqual(CartImpl.this) && items == that.items
        case _ => false
      }
    }

    override def hashCode() = {
      val prime = 41
      prime + cartItems.hashCode
    }
    
    override def toString = cartItems.toString
  }
}
