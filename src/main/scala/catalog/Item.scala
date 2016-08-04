package catalog

class Item(val name: String, val id: Long) extends Equals {
  def canEqual(other: Any) = {
    other.isInstanceOf[catalog.Item]
  }

  override def equals(other: Any) = {
    other match {
      case that: catalog.Item => that.canEqual(Item.this) && id == that.id
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime + id.hashCode
  }

  override def toString = id.toString
}