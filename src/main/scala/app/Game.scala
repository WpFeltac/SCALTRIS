package app

import app.Main.TEST_MODE
import app.PixelState.{MOVING, STATIC}
import app.ShapeSignature.{BAR, L, REVERSE_L, S, SQUARE, T}
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.*

import scala.annotation.tailrec
import scala.util.Random

final case class Game(signature: ShapeSignature, pixelMap: Map[(Int, Int), (Color, PixelState)], shapeList: List[Shape], cellSize: Int, gridBound: Int, TEST_MODE: Boolean) {
    def draw(): List[Rectangle] = {

        val drawList = pixelMap.map(p =>
            new Rectangle {
                x = p._1._1 * cellSize
                y = p._1._2 * cellSize
                width = cellSize
                height = cellSize
                fill = p._2._1
            }
        ).toList

        drawList
          // BASE
          :+ Rectangle(0, 600, 630, 60)
          // BANDE GAUCHE
          :+ Rectangle(0, 0, 30, 600)
          // BANDE DROITE
          :+ Rectangle(600, 0, 30, 600)
    }

    def play(direction: Direction): Game = {

        val nextShape = getNextMovingListFromSignature

        val moveResult = shapeList.foldLeft((List[Shape](), pixelMap)) { (prev, element) =>
            // If the shape is moving
            if(element.pixels.forall(p => p.state == MOVING)) {
                // If shape contains a pixel that will be blocked on next move by another static pixel
                if(element.pixels.exists(p => p.position._2 + 1 >= gridBound ||
                    prev._2.exists(e => e._1 == (p.position._1, p.position._2 + 1) && e._2._2 == STATIC))
                ) {
                    // Shape can't go down any further
                    val postMoveShapeList = prev._1 :+ Shape(element.pixels.map(p =>
                        p.copy(state = STATIC))
                    )
                    val postMovePixelMap = element.pixels.foldLeft(prev._2) { (prevMap, pixel) =>
                        prevMap.updated(pixel.position, (pixel.color, STATIC))
                    }

                    // Full line check
                    val postLineCheckMap = postMovePixelMap.map(e =>
                        // If current pixel line is a full one
                        // TODO : Edit this process
                        if(postMovePixelMap.count(me => me._1._2 == e._1._2) == 19) {                            
                            e.copy(_2 = e._2.copy(_2 = MOVING))                            
                        }
                        else {
                            e
                        }
                    )

                    (postMoveShapeList, postLineCheckMap)
                }
                else {
                    val isLeftMoveBlocked =
                        prev._2.exists(p =>
                            p._1._1 - 1 <= 0 ||
                              pixelMap.exists(e => e._1 == (p._1._1 - 1, p._1._2 + 1) && e._2._2 == STATIC)
                        )

                    val isRightMoveBlocked =
                        prev._2.exists(p =>
                            p._1._1 + 1 >= gridBound ||
                              pixelMap.exists(e => e._1 == (p._1._1 + 1, p._1._2 + 1) && e._2._2 == STATIC)
                        )

                    // Moving pixel downwards and horizontally
                    val xDirection = direction match
                        case Direction.LEFT => if (!isLeftMoveBlocked) -1 else 0
                        case Direction.RIGHT => if (!isRightMoveBlocked) 1 else 0
                        case Direction.NONE => 0

                    val res = prev._2.removed(element._1)
                      .updated((element._1._1 + xDirection, element._1._2 + 1), element._2)

                    res
                }
            }
            else {
                prev
            }
        }

        copy(
            signature = if (!TEST_MODE) ShapeSignature.values(Random.nextInt(ShapeSignature.values.length)) else BAR,
            pixelList = moveResult._1,
            pixelMap = moveResult._2
        )
    }

    private def getNextMovingListFromSignature: Option[Shape] = {
        val randCoord = Coord(10, 0)
        val randColor = Color.rgb(Random.nextInt(256), Random.nextInt(256), Random.nextInt(256))

        if (pixelMap.exists(e => e._2._2 == MOVING)) None else signature match
            case SQUARE =>
                Some(
                    Shape(
                        List(
                            // BL
                            Pixel((randCoord.x, randCoord.y + 1), randColor, MOVING),
                            // TL
                            Pixel((randCoord.x, randCoord.y), randColor, MOVING),
                            // TR
                            Pixel((randCoord.x + 1, randCoord.y), randColor, MOVING),
                            // BR
                            Pixel((randCoord.x + 1, randCoord.y + 1), randColor, MOVING)
                        )
                    )
                )                
            case T =>
                Some(
                    Shape(
                        List(
                            // TL
                            Pixel((randCoord.x - 1, randCoord.y), randColor, MOVING),
                            // TM
                            Pixel((randCoord.x, randCoord.y), randColor, MOVING),
                            // TR
                            Pixel((randCoord.x + 1, randCoord.y), randColor, MOVING),
                            // M
                            Pixel((randCoord.x, randCoord.y + 1), randColor, MOVING),
                        )
                    )
                )
            case L =>
                Some(
                    Shape(
                        List(
                            // TM
                            Pixel((randCoord.x, randCoord.y), randColor, MOVING),
                            // M
                            Pixel((randCoord.x, randCoord.y + 1), randColor, MOVING),
                            // BM
                            Pixel((randCoord.x, randCoord.y + 2), randColor, MOVING),
                            // BR
                            Pixel((randCoord.x + 1, randCoord.y + 2), randColor, MOVING),
                        )
                    )
                )
            case REVERSE_L =>
                Some(
                    Shape(
                        List(
                            // TM
                            Pixel((randCoord.x, randCoord.y), randColor, MOVING),
                            // M
                            Pixel((randCoord.x, randCoord.y + 1), randColor, MOVING),
                            // BM
                            Pixel((randCoord.x, randCoord.y + 2), randColor, MOVING),
                            // BL
                            Pixel((randCoord.x - 1, randCoord.y + 2), randColor, MOVING),
                        )
                    )
                )
            case BAR =>                
                Some(
                    Shape(
                        List(
                            // TM
                            Pixel((randCoord.x, randCoord.y), randColor, MOVING),
                            // M
                            Pixel((randCoord.x, randCoord.y + 1), randColor, MOVING),
                            // BM
                            Pixel((randCoord.x, randCoord.y + 2), randColor, MOVING),
                        )
                    )
                )
            case S =>
                Some(
                    Shape(
                        List(
                            // T
                            Pixel((randCoord.x, randCoord.y), randColor, MOVING),
                            // ML
                            Pixel((randCoord.x, randCoord.y + 1), randColor, MOVING),
                            // MR
                            Pixel((randCoord.x + 1, randCoord.y + 1), randColor, MOVING),
                            // B
                            Pixel((randCoord.x + 1, randCoord.y + 2), randColor, MOVING),
                        )
                    )
                )
    }
}
