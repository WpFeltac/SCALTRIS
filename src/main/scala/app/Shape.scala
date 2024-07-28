package app

import scalafx.scene.paint.Color

final case class Shape(position: (Int, Int), signature: ShapeSignature, color: Color, pixels: List[Pixel])
