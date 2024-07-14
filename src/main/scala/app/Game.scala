package app

import app.Main.TEST_MODE
import app.PixelState.{MOVING, STATIC}
import app.ShapeSignature.{BAR, L, REVERSE_L, S, SQUARE, T}
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.*

import scala.annotation.tailrec
import scala.util.Random

final case class Game(signature: ShapeSignature, pixelMap: Map[(Int, Int), Pixel], cellSize: Int, gridBound: Int, TEST_MODE: Boolean) {
    def draw(): List[Rectangle] = {

        val pixelList = pixelMap.map(p =>
            new Rectangle {
                x = p._1._1 * cellSize
                y = p._1._2 * cellSize
                width = cellSize
                height = cellSize
                fill = p._2.color
            }
        ).toList

        pixelList
          // BASE
          :+ Rectangle(0, 600, 630, 60)
          // BANDE GAUCHE
          :+ Rectangle(0, 0, 30, 600)
          // BANDE DROITE
          :+ Rectangle(600, 0, 30, 600)
    }

    def play(direction: Direction): Game = {

        val newShapeMap = getNextMapWithShape(pixelMap)
        println(newShapeMap)

        val moveResult = newShapeMap.foldLeft(newShapeMap) { (prevMap, element) =>
            if(element._2.state == MOVING) {
                if(element._1._2 + 1 >= gridBound ||
                  prevMap.exists(e => e._1 == (element._1._1, element._1._2 + 1) && e._2.color != element._2.color)
                ) {
                    // Pixel can't go down any further
                    val postPixelMoveMap = prevMap.updated(element._1, element._2.copy(state = STATIC))

                    // Full line check
                    val postLineCheckMap = postPixelMoveMap.map(e =>
                        if(postPixelMoveMap.count(me => me._1._2 == e._1._2) == 19) {
                            e.copy(_2 = e._2.copy(state = MOVING))
                        }
                        else {
                            e
                        }
                    )

                    postLineCheckMap
                }
                else {
                    val isLeftMoveBlocked =
                        prevMap.exists(p =>
                            p._1._1 - 1 <= 0 ||
                              pixelMap.exists(e => e._1 == (p._1._1 - 1, p._1._2 + 1) && e._2.color != p._2.color)
                        )

                    val isRightMoveBlocked =
                        prevMap.exists(p =>
                            p._1._1 + 1 >= gridBound ||
                              pixelMap.exists(e => e._1 == (p._1._1 + 1, p._1._2 + 1) && e._2.color != p._2.color)
                        )

                    // Moving pixel downwards and horizontally
                    val xDirection = direction match
                        case Direction.LEFT => if (!isLeftMoveBlocked) -1 else 0
                        case Direction.RIGHT => if (!isRightMoveBlocked) 1 else 0
                        case Direction.NONE => 0

                    val res = prevMap.removed(element._1)
                      .updated((element._1._1 + xDirection, element._1._2 + 1), element._2)

                    res
                }
            }
            else {
                prevMap
            }
        }

        copy(
            signature = if (!TEST_MODE) ShapeSignature.values(Random.nextInt(ShapeSignature.values.length)) else BAR,
            pixelMap = moveResult
        )
    }

    private def getNextMapWithShape(pixelMap: Map[(Int, Int), Pixel]): Map[(Int, Int), Pixel] = {
        val randCoord = Coord(10, 0)
        val randColor = Color.rgb(Random.nextInt(256), Random.nextInt(256), Random.nextInt(256))

        if (pixelMap.exists(e => e._2.state == MOVING)) pixelMap else signature match
            case SQUARE =>
                // BL
                pixelMap.updated((randCoord.x, randCoord.y + 1), Pixel(randColor, MOVING))
                // TL
                  .updated((randCoord.x, randCoord.y), Pixel(randColor, MOVING))
                // TR
                  .updated((randCoord.x + 1, randCoord.y), Pixel(randColor, MOVING))
                // BR
                  .updated((randCoord.x + 1, randCoord.y + 1), Pixel(randColor, MOVING))
            case T =>
                // TL
                pixelMap.updated((randCoord.x - 1, randCoord.y), Pixel(randColor, MOVING))
                // TM
                  .updated((randCoord.x, randCoord.y), Pixel(randColor, MOVING))
                // TR
                  .updated((randCoord.x + 1, randCoord.y), Pixel(randColor, MOVING))
                // M
                  .updated((randCoord.x, randCoord.y + 1), Pixel(randColor, MOVING))
            case L =>
                // TM
                pixelMap.updated((randCoord.x, randCoord.y), Pixel(randColor, MOVING))
                // M
                  .updated((randCoord.x, randCoord.y + 1), Pixel(randColor, MOVING))
                // BM
                  .updated((randCoord.x, randCoord.y + 2), Pixel(randColor, MOVING))
                // BR
                  .updated((randCoord.x + 1, randCoord.y + 2), Pixel(randColor, MOVING))
            case REVERSE_L =>
                // TM
                pixelMap.updated((randCoord.x, randCoord.y), Pixel(randColor, MOVING))
                // M
                  .updated((randCoord.x, randCoord.y + 1), Pixel(randColor, MOVING))
                // BM
                  .updated((randCoord.x, randCoord.y + 2), Pixel(randColor, MOVING))
                // BL
                  .updated((randCoord.x - 1, randCoord.y + 2), Pixel(randColor, MOVING))
            case BAR =>
                // TM
                pixelMap.updated((randCoord.x, randCoord.y), Pixel(randColor, MOVING))
                // M
                  .updated((randCoord.x, randCoord.y + 1), Pixel(randColor, MOVING))
                // BM
                  .updated((randCoord.x, randCoord.y + 2), Pixel(randColor, MOVING))
            case S =>
                // T
                pixelMap.updated((randCoord.x, randCoord.y), Pixel(randColor, MOVING))
                // ML
                  .updated((randCoord.x, randCoord.y + 1), Pixel(randColor, MOVING))
                // MR
                  .updated((randCoord.x + 1, randCoord.y + 1), Pixel(randColor, MOVING))
                // B
                  .updated((randCoord.x + 1, randCoord.y + 2), Pixel(randColor, MOVING))
    }
}
