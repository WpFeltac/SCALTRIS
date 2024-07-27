package app

import app.Main.TEST_MODE
import app.PixelState.{MOVING, STATIC}
import app.ShapeSignature.{BAR, L, REVERSE_L, S, SQUARE, T}
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.*

import scala.annotation.{static, tailrec}
import scala.language.postfixOps
import scala.util.Random

final case class Game(signature: ShapeSignature, pixelMap: Map[(Int, Int), Pixel], pixelList: List[Pixel], groupId: Int, cellSize: Int, gridBound: Int, TEST_MODE: Boolean) {
    def draw(): List[Rectangle] = {

        val drawList = pixelList.map(p =>
            new Rectangle {
                x = p.position._1 * cellSize
                y = p.position._2 * cellSize
                width = cellSize
                height = cellSize
                fill = p.color
            }
        )

        drawList
          // BASE
          :+ Rectangle(0, 600, 630, 60)
          // BANDE GAUCHE
          :+ Rectangle(0, 0, 30, 600)
          // BANDE DROITE
          :+ Rectangle(600, 0, 30, 600)
    }

    def play(direction: Direction): Game = {

        val nextShapeList = getNextMovingListFromSignature

        val moveResult = nextShapeList.foldLeft((List[Pixel](), pixelMap)) { (prev, pixel) =>
            // If the shape is moving
            if(pixel.state == MOVING) {
                // If pixel will be blocked on next move by another static pixel or bound
                if(pixel.position._2 + 1 >= gridBound ||
                    prev._2.exists(pm => pm._2.position == (pixel.position._1, pixel.position._2 + 1) && pm._2.state == STATIC)
                ) {
                    // Pixel and all in its group can't go down any further
                    val staticSetResult = setGroupStateToStatic(pixel.groupId, nextShapeList, prev._2)

                    // Full line check
                    val postLineCheckMap = staticSetResult._2.map(e =>
                        // If current pixel line is a full one
                        // TODO : Edit this process
                        if(staticSetResult._2.count(me => me._1._2 == e._1._2) == 19) {                            
                            e.copy(_2 = e._2.copy(state = MOVING))                            
                        }
                        else {
                            e
                        }
                    )

                    (staticSetResult._1, postLineCheckMap)
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
                    
                    val newList = prev._1 :+ pixel.copy(position = (pixel.position._1 + xDirection, pixel.position._2 + 1))
                    val newMap = prev._2.removed(pixel.position)
                      .updated(
                          (pixel.position._1 + xDirection, pixel.position._2 + 1),
                          pixel.copy(position = (pixel.position._1 + xDirection, pixel.position._2 + 1)
                      )
                    )

                    (newList, newMap)
                }
            }
            else {
                prev
            }
        }

        copy(
            signature = if (!TEST_MODE) ShapeSignature.values(Random.nextInt(ShapeSignature.values.length)) else BAR,
            pixelList = moveResult._1,
            pixelMap = moveResult._2,
            groupId = if(nextShapeList.length > pixelList.length) groupId + 1 else groupId
        )
    }

    private def setGroupStateToStatic(groupId: Int, recPixelList: List[Pixel], recPixelMap: Map[(Int, Int), Pixel], index: Int = 0): (List[Pixel], Map[(Int, Int), Pixel]) = {
        if(index < recPixelList.length) {
            val currentPixel = recPixelList(index)
            if(currentPixel.groupId == groupId && currentPixel.state == MOVING) {
                
                val newList = recPixelList.filterNot(p => p == currentPixel) :+ currentPixel.copy(state = STATIC)
                val newMap = recPixelMap.updated(currentPixel.position, currentPixel.copy(state = STATIC))

                setGroupStateToStatic(groupId, newList, newMap, index + 1)
            }
            else {
                setGroupStateToStatic(groupId, recPixelList, recPixelMap, index)
            }
        }
        else {
            (recPixelList, recPixelMap)
        }
    }

    private def getNextMovingListFromSignature: List[Pixel] = {
        val randCoord = (10, 0)
        val randColor = Color.rgb(Random.nextInt(256), Random.nextInt(256), Random.nextInt(256))

        if (pixelMap.exists(e => e._2.state == MOVING)) pixelList
        else {
            val newPixels = signature match
                case SQUARE =>
                    List(
                        // BL
                        Pixel((randCoord._1, randCoord._2 + 1), randColor, MOVING, groupId),
                        // TL
                        Pixel((randCoord._1, randCoord._2), randColor, MOVING, groupId),
                        // TR
                        Pixel((randCoord._1 + 1, randCoord._2), randColor, MOVING, groupId),
                        // BR
                        Pixel((randCoord._1 + 1, randCoord._2 + 1), randColor, MOVING, groupId)
                    )               
                case T =>
                    List(
                        // TL
                        Pixel((randCoord._1 - 1, randCoord._2), randColor, MOVING, groupId),
                        // TM
                        Pixel((randCoord._1, randCoord._2), randColor, MOVING, groupId),
                        // TR
                        Pixel((randCoord._1 + 1, randCoord._2), randColor, MOVING, groupId),
                        // M
                        Pixel((randCoord._1, randCoord._2 + 1), randColor, MOVING, groupId),
                    )
                case L =>
                    List(
                        // TM
                        Pixel((randCoord._1, randCoord._2), randColor, MOVING, groupId),
                        // M
                        Pixel((randCoord._1, randCoord._2 + 1), randColor, MOVING, groupId),
                        // BM
                        Pixel((randCoord._1, randCoord._2 + 2), randColor, MOVING, groupId),
                        // BR
                        Pixel((randCoord._1 + 1, randCoord._2 + 2), randColor, MOVING, groupId),
                    )
                case REVERSE_L =>
                    List(
                        // TM
                        Pixel((randCoord._1, randCoord._2), randColor, MOVING, groupId),
                        // M
                        Pixel((randCoord._1, randCoord._2 + 1), randColor, MOVING, groupId),
                        // BM
                        Pixel((randCoord._1, randCoord._2 + 2), randColor, MOVING, groupId),
                        // BL
                        Pixel((randCoord._1 - 1, randCoord._2 + 2), randColor, MOVING, groupId),
                    )
                case BAR =>
                    List(
                        // TM
                        Pixel((randCoord._1, randCoord._2), randColor, MOVING, groupId),
                        // M
                        Pixel((randCoord._1, randCoord._2 + 1), randColor, MOVING, groupId),
                        // BM
                        Pixel((randCoord._1, randCoord._2 + 2), randColor, MOVING, groupId),
                    )
                case S =>
                    List(
                        // T
                        Pixel((randCoord._1, randCoord._2), randColor, MOVING, groupId),
                        // ML
                        Pixel((randCoord._1, randCoord._2 + 1), randColor, MOVING, groupId),
                        // MR
                        Pixel((randCoord._1 + 1, randCoord._2 + 1), randColor, MOVING, groupId),
                        // B
                        Pixel((randCoord._1 + 1, randCoord._2 + 2), randColor, MOVING, groupId),
                    )
                    
            pixelList ::: newPixels
        }
    }
}
