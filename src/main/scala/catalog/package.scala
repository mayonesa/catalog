package object catalog {

  type Price = BigDecimal
  type Quantity = Int
  type Bundle = Set[(Item, Quantity)]
}