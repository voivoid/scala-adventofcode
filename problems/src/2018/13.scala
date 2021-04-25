package adventOfCode.problems
package year2018

object problem13 extends baseProblem {

  import adventOfCode.utils.path.{Bearing, Turn, bearingLocDelta, doTurn}
  import adventOfCode.utils.geo.Point
  import scala.collection.immutable.TreeMap

  override def solve1(input: Input): String = {
    solve(input, findFirstCrash)
  }

  override def solve2(input: Input): String = {
    solve(input, findLastCart)
  }

  private type Coord = Point[Int]
  private type Tracks = Function[Coord, Char]
  private type CartsMap = TreeMap[Coord, Cart]
  private case class Cart(bearing: Bearing.Bearing, turn: Turn.Turn)

  private def solve(input: Input, findLocation: (Tracks, CartsMap) => Coord): String = {
    val lines = input.getLines().buffered
    val width = lines.head.size

    val tracksAndCarts = lines.mkString
    val tracks = tracksAndCarts.map(cartToTrack)
    val tracksGetter = (c: Coord) => tracks(c.y * width + c.x)

    val carts = tracksAndCarts.zipWithIndex.filter { case (c, _) => isCart(c) }
    val cartsMap = makeCartsMap(carts, width)

    val location = findLocation(tracksGetter, cartsMap)
    s"${location.x},${location.y}"
  }

  private def updateMap(coord: Coord, cart: Cart, tracks: Tracks, cartsMap: CartsMap): (CartsMap, Option[Coord]) = {
    val (updatedCoord, updatedCart) = moveCart(coord, cart, tracks)

    if (cartsMap.contains(updatedCoord)) { // is a crash?
      ((cartsMap - coord) - updatedCoord, Some(updatedCoord))
    } else if (cartsMap.contains(coord)) { // check the cart wasn't deleted due to a previous crash
      (cartsMap - coord + (updatedCoord -> updatedCart), None)
    } else (cartsMap, None)
  }

  private def runTick(tracks: Tracks, cartsMap: CartsMap): CartsMap = {
    cartsMap.foldLeft(cartsMap) { case (accMap, (coord, cart)) =>
      updateMap(coord, cart, tracks, accMap)._1
    }
  }

  private def findFirstCrash(tracks: Tracks, cartsMap: CartsMap): Coord = {
    import adventOfCode.utils.algorithms.IteratorSlidingTuple

    val (tickWithCrash, _) = Iterator
      .iterate(cartsMap)(runTick(tracks, _))
      .sliding2
      .find { case (t1map, t2map) =>
        t1map.size != t2map.size
      }
      .get

    val tickUpdateStates = tickWithCrash.iterator.scanLeft((tickWithCrash, None: Option[Coord])) { case ((accMap, _), (coord, cart)) =>
      updateMap(coord, cart, tracks, accMap)
    }

    tickUpdateStates.collectFirst { case (_, Some(crash)) => crash }.get
  }

  private def findLastCart(tracks: Tracks, cartsMap: CartsMap): Coord = {
    val mapWithSingleCart = Iterator.iterate(cartsMap)(runTick(tracks, _)).find(_.size == 1).get
    mapWithSingleCart.head._1
  }

  private def moveCart(coord: Coord, cart: Cart, tracks: Tracks): (Coord, Cart) = {
    val track = tracks(coord)
    val updatedCart = track match {

      case t if isStraight(t) => cart

      case t if isCurve(t) => {
        val nextBearing = (t, cart.bearing) match {
          case ('\\', Bearing.North) => Bearing.West
          case ('\\', Bearing.East)  => Bearing.South
          case ('\\', Bearing.South) => Bearing.East
          case ('\\', Bearing.West)  => Bearing.North

          case ('/', Bearing.North) => Bearing.East
          case ('/', Bearing.East)  => Bearing.North
          case ('/', Bearing.South) => Bearing.West
          case ('/', Bearing.West)  => Bearing.South
        }

        cart.copy(bearing = nextBearing)
      }

      case t if isIntersection(t) => {
        val nextTurn = cart.turn match {
          case Turn.Left  => Turn.None
          case Turn.None  => Turn.Right
          case Turn.Right => Turn.Left
        }
        val nextBearing = doTurn(cart.bearing, cart.turn)

        Cart(nextBearing, nextTurn)
      }

      case _ => sys.error("unexpected track")
    }

    val delta = bearingLocDelta(updatedCart.bearing, 1)
    (coord + delta.copy(y = -delta.y), updatedCart)
  }

  private def makeCartsMap(carts: Seq[(Char, Int)], width: Int): CartsMap = {
    implicit val ordering = Ordering[Coord].on((c: Coord) => Point(c.y, c.x))

    carts
      .map { case (cartChar, index) =>
        val coord = indexToCoord(index, width)
        val cart = Cart(getBearing(cartChar), Turn.Left)
        coord -> cart
      }
      .to(TreeMap)
  }

  private def indexToCoord(index: Int, width: Int): Coord = {
    import scala.math.Integral.Implicits._
    val (y, x) = index /% width
    Point(x, y)
  }

  private def getBearing(c: Char): Bearing.Bearing = {
    c match {
      case '^' => Bearing.North
      case '<' => Bearing.West
      case '>' => Bearing.East
      case 'v' => Bearing.South
      case _   => sys.error("unexpected cart char")
    }
  }

  private def isCart(c: Char): Boolean = {
    c == '^' || c == '>' || c == '<' || c == 'v'
  }

  private def isStraight(c: Char) = c == '|' || c == '-'
  private def isCurve(c: Char) = c == '\\' || c == '/'
  private def isIntersection(c: Char) = c == '+'

  private def cartToTrack(c: Char): Char = c match {
    case c if c == '^' || c == 'v' => '|'
    case c if c == '<' || c == '>' => '-'
    case c                         => c
  }

}
