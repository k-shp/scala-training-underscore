package day1

sealed trait Color

final case class RBGColor(red: Double, green: Double, blue: Double)
    extends Color

final case class CMYKColor(
    cyan: Double,
    magenta: Double,
    yellow: Double,
    kBlack: Double
) extends Color
