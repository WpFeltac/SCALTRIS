package app

import scalafx.scene.paint.Color

final case class Pixel(position: (Int, Int), color: Color, state: PixelState, groupId: Int)
