package adventOfCode.utils

package object ocr {

  def decodeChars(pixels: Seq[Char], width: Int, height: Int, charWidth: Int): String = {
    val charsNum = width / charWidth

    val indexedPixels = pixels.zipWithIndex

    val charsPixels = (0 until charsNum).iterator.map(charN => {
      val fromPixelIndex = charN * charWidth
      val toPixelindex = fromPixelIndex + charWidth
      val pixelRange = fromPixelIndex until toPixelindex

      indexedPixels.withFilter { case (_, pixelIndex) => pixelRange.contains(pixelIndex % width) }.map(_._1)
    })

    val charMap = height match {
      case 6  => pixels6ToCharMap
      case 10 => pixels10ToCharMap
      case _  => sys.error("unknown charmap")
    }

    charsPixels.map(charMap).mkString
  }

  private def pixels10ToCharMap: Map[Seq[Char], Char] = {

    val a = """..██....
              |.█..█...
              |█....█..
              |█....█..
              |█....█..
              |██████..
              |█....█..
              |█....█..
              |█....█..
              |█....█..""".stripMargin

    val b = """█████...
              |█....█..
              |█....█..
              |█....█..
              |█████...
              |█....█..
              |█....█..
              |█....█..
              |█....█..
              |█████...""".stripMargin

    val e = """██████..
              |█.......
              |█.......
              |█.......
              |█████...
              |█.......
              |█.......
              |█.......
              |█.......
              |██████..""".stripMargin

    val f = """██████..
              |█.......
              |█.......
              |█.......
              |█████...
              |█.......
              |█.......
              |█.......
              |█.......
              |█.......""".stripMargin

    val g = """.████...
              |█....█..
              |█.......
              |█.......
              |█.......
              |█..███..
              |█....█..
              |█....█..
              |█...██..
              |.███.█..""".stripMargin

    val h = """█....█..
              |█....█..
              |█....█..
              |█....█..
              |██████..
              |█....█..
              |█....█..
              |█....█..
              |█....█..
              |█....█..""".stripMargin

    val k = """█....█..
              |█...█...
              |█..█....
              |█.█.....
              |██......
              |██......
              |█.█.....
              |█..█....
              |█...█...
              |█....█..""".stripMargin

    val l = """█.......
              |█.......
              |█.......
              |█.......
              |█.......
              |█.......
              |█.......
              |█.......
              |█.......
              |██████..""".stripMargin

    val letters = List(a -> 'A', b -> 'B', e -> 'E', f -> 'F', g -> 'G', h -> 'H', k -> 'K', l -> 'L')

    def mapChar(c: Char) = c match {
      case '█' => '1'
      case '.' => '0'
    }

    letters.map { case (pixels, letter) => pixels.iterator.filterNot(_.isWhitespace).map(mapChar).toSeq -> letter }.toMap
  }

  private def pixels6ToCharMap: Map[Seq[Char], Char] = {
    val █ = '1'
    val - = '0'

    // format: off

    val chars: Seq[(Seq[Char], Char)] = Seq(

      Seq( -, █, █, -, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, █, █, █, -,
           █, -, -, █, -,
           █, -, -, █, - ) -> 'A',

      Seq( █, █, █, -, -,
           █, -, -, █, -,
           █, █, █, -, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, █, █, -, - ) -> 'B',

      Seq( -, █, █, -, -,
           █, -, -, █, -,
           █, -, -, -, -,
           █, -, -, -, -,
           █, -, -, █, -,
           -, █, █, -, -) -> 'C',

      Seq( █, █, █, █, -,
           █, -, -, -, -,
           █, █, █, -, -,
           █, -, -, -, -,
           █, -, -, -, -,
           █, █, █, █, -) -> 'E',

      Seq( █, █, █, █, -,
           █, -, -, -, -,
           █, █, █, -, -,
           █, -, -, -, -,
           █, -, -, -, -,
           █, -, -, -, - ) -> 'F',

      Seq( -, █, █, -, -,
           █, -, -, █, -,
           █, -, -, -, -,
           █, -, █, █, -,
           █, -, -, █, -,
           -, █, █, █, -) -> 'G',

      Seq( █, -, -, █, -,
           █, -, -, █, -,
           █, █, █, █, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, -, -, █, - ) -> 'H',

      Seq( -, █, █, █, -,
           -, -, █, -, -,
           -, -, █, -, -,
           -, -, █, -, -,
           -, -, █, -, -,
           -, █, █, █, - ) -> 'I',

      Seq( -, -, █, █, -,
           -, -, -, █, -,
           -, -, -, █, -,
           -, -, -, █, -,
           █, -, -, █, -,
           -, █, █, -, - ) -> 'J',

      Seq( █, -, -, █, -,
           █, -, █, -, -,
           █, █, -, -, -,
           █, -, █, -, -,
           █, -, █, -, -,
           █, -, -, █, -) -> 'K',

      Seq( █, -, -, -, -,
           █, -, -, -, -,
           █, -, -, -, -,
           █, -, -, -, -,
           █, -, -, -, -,
           █, █, █, █, - ) -> 'L',

      Seq( -, █, █, -, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, -, -, █, -,
           -, █, █, -, -) -> 'O',

      Seq( █, █, █, -, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, █, █, -, -,
           █, -, -, -, -,
           █, -, -, -, -) -> 'P',

      Seq( █, █, █, -, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, █, █, -, -,
           █, -, █, -, -,
           █, -, -, █, -) -> 'R',

      Seq( █, -, -, █, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, -, -, █, -,
           -, █, █, -, -) -> 'U',

      Seq( █, -, -, -, █,
           █, -, -, -, █,
           -, █, -, █, -,
           -, -, █, -, -,
           -, -, █, -, -,
           -, -, █, -, - ) -> 'Y',

      Seq( █, █, █, █, -,
           -, -, -, █, -,
           -, -, █, -, -,
           -, █, -, -, -,
           █, -, -, -, -,
           █, █, █, █, - ) -> 'Z'
    )

    // format: on

    chars.toMap
  }

}
