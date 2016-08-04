package catalog

import scala.annotation.tailrec
import scala.language.postfixOps

trait Catalog {
  def price(cart: Set[(Item, Quantity)]): Price
}

object Catalog {
  def apply(itemPrices: Set[(Item, Price)], bundlePrices: Set[(Bundle, Price)]): Catalog =
    new CatalogImpl(itemPrices, bundlePrices)

  private class CatalogImpl(itemPrices: Set[(Item, Price)], bundlePrices: Set[(Bundle, Price)]) extends Catalog {
 
    private val itemPriceMap = itemPrices map {
      case (item, price) => (item.id, price)
    } toMap

    def price(cartItems: Set[(Item, Quantity)]): Price = {
      val cart = Cart(cartItems)

      def bundleExhaustiveApplicabilityMaterializer(bp: (Bundle, Price)) = {
        @tailrec
        def loop(c: Cart, acc: Vector[(Bundle, Price)]): Vector[(Bundle, Price)] = {
          val b = bp._1
          if (bundleApplicable(c, b)) loop(c -- b, acc :+ bp)
          else acc
        }
        loop(cart, Vector.empty[(Bundle, Price)])
      }

      def lowestApplicableBundleSeqPermuationPrice(lowestPrice: Seq[(Bundle, Price)] => Option[Price]) = {
        val applicableBundlesExhaustively = bundlePrices.toVector.flatMap(bundleExhaustiveApplicabilityMaterializer)
        applicableBundlesExhaustively.permutations.toVector.par.map(lowestPrice).fold(None) {
          (minPriceOpt, priceOpt) =>
            priceOpt match {
              case None => minPriceOpt
              case Some(price) => minPriceOpt match {
                case None => priceOpt
                case Some(minPrice) => Some { price min minPrice }
              }
            }
        }
      }

      val lowestBundledPrice = lowestApplicableBundleSeqPermuationPrice { bundleSeqPermutation =>
        val (lowestPrice, cartRemains) = bundleSeqPermutation.foldLeft((None: Option[Price], cart)) {
          case ((accPriceOpt, updatedCart), (bundle, bundlePrice)) =>
            if (bundleApplicable(updatedCart, bundle))
              (Some { bundlePrice + accPriceOpt.getOrElse(0) }, updatedCart -- bundle)
            else (accPriceOpt, updatedCart)
        }
        lowestPrice map { _ + price(cartRemains) }
      }
      lowestBundledPrice.getOrElse(price(cart))
    }

    private def price(cart: Cart) = {
      def price(iq: (Item, Quantity)) = iq match {
        case (item, quant) => itemPriceMap(item.id) * quant
      }
      cart.items.map(price).fold(BigDecimal(0)) { _ + _ }
    }

    private def bundleApplicable(c: Cart, b: Bundle) = {
      @tailrec
      def loop(bRemaining: List[(Item, Quantity)]): Boolean = bRemaining match {
        case Nil => true
        case (bundleItem, bundleItemQuant) :: xs =>
          if (c.quantity(bundleItem) >= bundleItemQuant) loop(xs) else false
      }
      loop(b toList)
    }
  }
}