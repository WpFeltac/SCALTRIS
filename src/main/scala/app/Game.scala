package app

import app.Main.TEST_MODE
import app.ShapeSignature.{BAR, L, REVERSE_L, S, SQUARE, T}
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.*

import scala.annotation.tailrec
import scala.util.Random

final case class Game(signature: ShapeSignature, movingList: List[Pixel], pixelMap: Map[(Int, Int), Pixel], cellSize: Int, gridBound: Int, TEST_MODE: Boolean) {
    def draw(): List[Rectangle] = {

        val drawMovingList = movingList.map(m =>
            new Rectangle {
                x = m.position.x * cellSize
                y = m.position.y * cellSize
                width = cellSize
                height = cellSize
                fill = m.color
            }
        )

        val pixelList = pixelMap.map(p =>
            new Rectangle {
                x = p._2.position.x * cellSize
                y = p._2.position.y * cellSize
                width = cellSize
                height = cellSize
                fill = p._2.color
            }
        ).toList

        (drawMovingList ::: pixelList)
          // BASE
          :+ Rectangle(0, 600, 630, 60)
          // BANDE GAUCHE
          :+ Rectangle(0, 0, 30, 600)
          // BANDE DROITE
          :+ Rectangle(600, 0, 30, 600)
    }

    def play(direction: Direction): Game = {

        val randCoord = Coord(10, 0)
        val randColor = Color.rgb(Random.nextInt(256), Random.nextInt(256), Random.nextInt(256))

        val movingPixels = if(movingList.nonEmpty) movingList else signature match
            case SQUARE =>
                List(
                    // BL
                    Pixel(Coord(randCoord.x, randCoord.y + 1), randColor),
                    // TL
                    Pixel(Coord(randCoord.x, randCoord.y), randColor),
                    // TR
                    Pixel(Coord(randCoord.x + 1, randCoord.y), randColor),
                    // BR
                    Pixel(Coord(randCoord.x + 1, randCoord.y + 1), randColor)
                )
            case T =>
                List(
                    // TL
                    Pixel(Coord(randCoord.x - 1, randCoord.y), randColor),
                    // TM
                    Pixel(Coord(randCoord.x, randCoord.y), randColor),
                    // TR
                    Pixel(Coord(randCoord.x + 1, randCoord.y), randColor),
                    // M
                    Pixel(Coord(randCoord.x, randCoord.y + 1), randColor),
                )
            case L =>
                List(
                    // TM
                    Pixel(Coord(randCoord.x, randCoord.y), randColor),
                    // M
                    Pixel(Coord(randCoord.x, randCoord.y + 1), randColor),
                    // BM
                    Pixel(Coord(randCoord.x, randCoord.y + 2), randColor),
                    // BR
                    Pixel(Coord(randCoord.x + 1, randCoord.y + 2), randColor),
                )
            case REVERSE_L =>
                List(
                    // TM
                    Pixel(Coord(randCoord.x, randCoord.y), randColor),
                    // M
                    Pixel(Coord(randCoord.x, randCoord.y + 1), randColor),
                    // BM
                    Pixel(Coord(randCoord.x, randCoord.y + 2), randColor),
                    // BL
                    Pixel(Coord(randCoord.x - 1, randCoord.y + 2), randColor),
                )
            case BAR =>
                List(
                    // TM
                    Pixel(Coord(randCoord.x, randCoord.y), randColor),
                    // M
                    Pixel(Coord(randCoord.x, randCoord.y + 1), randColor),
                    // BM
                    Pixel(Coord(randCoord.x, randCoord.y + 2), randColor),
                )
            case S =>
                List(
                    // T
                    Pixel(Coord(randCoord.x, randCoord.y), randColor),
                    // ML
                    Pixel(Coord(randCoord.x, randCoord.y + 1), randColor),
                    // MR
                    Pixel(Coord(randCoord.x + 1, randCoord.y + 1), randColor),
                    // B
                    Pixel(Coord(randCoord.x + 1, randCoord.y + 2), randColor),
                )

        val isNextMoveBlocked =
            movingPixels.exists(p =>
                p._1._2 + 1 >= gridBound ||
                  pixelMap.contains((p._1._1, p._1._2 + 1))
            )

        // Moving shape will stop and we generate a new falling one
        if(isNextMoveBlocked) {

            val postMovePixelMap = addPixelsToMap(pixelMap, movingPixels.map(p => ((p.position.x, p.position.y), p)), 0)

            // Vérification ligne
            val fullLineCounters = (1 to 19).map(i =>
                postMovePixelMap.count(m => m._1._2 == i)
            )

            if(fullLineCounters.contains(19)) {
                val fullLineY = fullLineCounters.indexOf(19) + 1

                println("Full line at " + fullLineY + " !")

                val postLinePixelMap = postMovePixelMap.filterNot(p => p._1._2 >= fullLineY).map(p =>
                    ((p._1._1, p._1._2 + 1), p._2.copy(position = Coord(p._2.position.x, p._2.position.y + 1)))
                )

                copy(
                    signature = if(!TEST_MODE) ShapeSignature.values(Random.nextInt(ShapeSignature.values.length)) else BAR,
                    movingList = List(),
                    pixelMap = postLinePixelMap
                )
            }
            else {
                copy(
                    signature = if(!TEST_MODE) ShapeSignature.values(Random.nextInt(ShapeSignature.values.length)) else BAR,
                    movingList = List(),
                    pixelMap = postMovePixelMap
                )
            }
        }
        else {
            // Déplacement latéral
            val isLeftMoveBlocked =
                movingPixels.exists(p =>
                    p._1._1 - 1 <= 0 ||
                      pixelMap.contains((p._1._1 - 1, p._1._2))
                )

            val isRightMoveBlocked =
                movingPixels.exists(p =>
                    p._1._1 + 1 >= gridBound ||
                      pixelMap.contains((p._1._1 + 1, p._1._2))
                )

            val xCoeff = direction match
                case Direction.LEFT => if(!isLeftMoveBlocked) -1 else 0
                case Direction.RIGHT => if(!isRightMoveBlocked) 1 else 0
                case Direction.NONE => 0

            copy(movingList = movingPixels.map(p => p.copy(position = Coord(p.position.x + xCoeff, p.position.y + 1))))
        }
    }

    @tailrec
    private def addPixelsToMap(pixelMap: Map[(Int, Int), Pixel], toAddList: List[((Int, Int), Pixel)], addIndex: Int): Map[(Int, Int), Pixel] = {
        if(addIndex < toAddList.length) {
            val newPixel = toAddList(addIndex)
            addPixelsToMap(pixelMap.updated(newPixel._1, newPixel._2), toAddList, addIndex + 1)
        }
        else {
            pixelMap
        }
    }
}
