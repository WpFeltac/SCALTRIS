package app

import app.PixelState.{FALLING, MOVING, STATIC}
import app.ShapeSignature.{BAR, L, REVERSE_L, S, SQUARE, T}
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.*

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.Random

final case class Game(movingShape: Option[Shape], pixelMap: Map[(Int, Int), (PixelState, Color)], cellSize: Int, gridBound: Int, lost: Boolean, TEST_MODE: Boolean) {
    def draw(): List[Rectangle] = {
        
        val shapeDrawList = if(movingShape.isDefined) {            
            movingShape.get.pixels.map(p => {
                new Rectangle {
                    x = p._1._1 * cellSize
                    y = p._1._2 * cellSize
                    width = cellSize
                    height = cellSize
                    fill = movingShape.get.color
                }
            })
        } else {
            List()
        }
        

        val drawList = pixelMap.map(p =>
            new Rectangle {
                x = p._1._1 * cellSize
                y = p._1._2 * cellSize
                width = cellSize
                height = cellSize
                fill = p._2._2
            }
        ).toList

        val drawLost = getGameOver()

        val content = if(!lost) shapeDrawList ::: drawList else drawLost

        content
          // BASE
          :+ Rectangle(0, 600, 630, 60)
          // BANDE GAUCHE
          :+ Rectangle(0, 0, 30, 600)
          // BANDE DROITE
          :+ Rectangle(600, 0, 30, 600)
    }

    def play(direction: Direction): Game = {

        val currentShape = if(movingShape.isDefined) movingShape.get else getNextShape

        if(currentShape.position == (10, 0) && pixelMap.contains(currentShape.position)) {
            println("Lost")

            copy (
                pixelMap = Map(),
                lost = true
            )
        }
        else if(isNextMoveCritical(currentShape)) {
            val nextPixelMap = currentShape.pixels.foldLeft(pixelMap)((prevMap, p) =>
                prevMap.updated(p.position, (STATIC, currentShape.color))
            )

            // Vérification de ligne complète + suppression et mouvement en conséquence
            val counts = (1 to 19).map(l => nextPixelMap.count(pm => pm._1._2 == l))
            val fullLinesIndexes = counts.zipWithIndex.filter((c, i) => c == 19).map(res => res._2 + 1)
            val postRemoveMap = nextPixelMap.filterNot(pm => fullLinesIndexes.contains(pm._1._2))

            val postDownMoveMap = fullLinesIndexes.foldLeft(postRemoveMap)((prev, li) =>
                prev.map(pm =>
                    if(pm._1._2 < li) {
                        pm.copy(_1 = (pm._1._1, pm._1._2 + 1))
                    } else {
                        pm
                    }
                )
            )

            copy(
                movingShape = None,
                pixelMap = postDownMoveMap
            )
        }
        else {
            val xDirection = direction match
                case Direction.LEFT => if (!isLeftBlocked(currentShape)) -1 else 0
                case Direction.RIGHT => if (!isRightBlocked(currentShape)) 1 else 0
                case Direction.NONE => 0

            copy(
                movingShape = Some(currentShape.copy(
                    position = (currentShape.position._1 + xDirection, currentShape.position._2 + 1),
                    pixels = currentShape.pixels.map(p => p.copy(position = (p.position._1 + xDirection, p.position._2 + 1)))
                )),
            )
        }
    }

    private def isLeftBlocked(shape: Shape): Boolean = {
        shape.signature match
            case SQUARE =>
                shape.position._1 - 1 <= 0 ||
                  pixelMap.contains((shape.position._1 - 1, shape.position._2)) ||
                  pixelMap.contains((shape.position._1 - 1, shape.position._2 + 1)) ||
                  pixelMap.contains((shape.position._1 - 1, shape.position._2 + 2))
            case T =>
                shape.position._1 - 1 <= 0  ||
                  pixelMap.contains((shape.position._1 - 1, shape.position._2)) ||
                  pixelMap.contains((shape.position._1, shape.position._2 + 1)) ||
                  pixelMap.contains((shape.position._1, shape.position._2 + 2))
            case L | BAR =>
                shape.position._1 - 1 <= 0 ||
                  pixelMap.contains((shape.position._1 - 1, shape.position._2)) ||
                  pixelMap.contains((shape.position._1 - 1, shape.position._2 + 1)) ||
                  pixelMap.contains((shape.position._1 - 1, shape.position._2 + 2)) ||
                  pixelMap.contains((shape.position._1 - 1, shape.position._2 + 3))
            case REVERSE_L =>
                shape.position._1 - 1 <= 0 ||
                  pixelMap.contains((shape.position._1 - 1, shape.position._2)) ||
                  pixelMap.contains((shape.position._1 - 1, shape.position._2 + 1)) ||
                  pixelMap.contains((shape.position._1 - 2, shape.position._2 + 2)) ||
                  pixelMap.contains((shape.position._1 - 2, shape.position._2 + 3))
            case S =>
                shape.position._1 - 1 <= 0  ||
                  pixelMap.contains((shape.position._1 - 1, shape.position._2)) ||
                  pixelMap.contains((shape.position._1 - 1, shape.position._2 + 1)) ||
                  pixelMap.contains((shape.position._1, shape.position._2 + 2)) ||
                  pixelMap.contains((shape.position._1, shape.position._2 + 3))
    }

    private def isRightBlocked(shape: Shape): Boolean = {
        shape.signature match
            case SQUARE =>
                shape.position._1 + 2 >= gridBound ||
                  pixelMap.contains((shape.position._1 + 2, shape.position._2)) ||
                  pixelMap.contains((shape.position._1 + 2, shape.position._2 + 1)) ||
                  pixelMap.contains((shape.position._1 + 2, shape.position._2 + 2))
            case T =>
                shape.position._1 + 3 >= gridBound ||
                  pixelMap.contains((shape.position._1 + 3, shape.position._2)) ||
                  pixelMap.contains((shape.position._1 + 2, shape.position._2 + 1)) ||
                  pixelMap.contains((shape.position._1 + 2, shape.position._2 + 2))
            case L =>
                shape.position._1 + 2 >= gridBound ||
                  pixelMap.contains((shape.position._1 + 1, shape.position._2)) ||
                  pixelMap.contains((shape.position._1 + 1, shape.position._2 + 1)) ||
                  pixelMap.contains((shape.position._1 + 2, shape.position._2 + 2)) ||
                  pixelMap.contains((shape.position._1 + 2, shape.position._2 + 3))
            case REVERSE_L | BAR =>
                shape.position._1 + 1 >= gridBound ||
                  pixelMap.contains((shape.position._1 + 1, shape.position._2)) ||
                  pixelMap.contains((shape.position._1 + 1, shape.position._2 + 1)) ||
                  pixelMap.contains((shape.position._1 + 1, shape.position._2 + 2)) ||
                  pixelMap.contains((shape.position._1 + 1, shape.position._2 + 3))
            case S =>
                shape.position._1 + 2 >= gridBound ||
                  pixelMap.contains((shape.position._1 + 1, shape.position._2)) ||
                  pixelMap.contains((shape.position._1 + 2, shape.position._2 + 1)) ||
                  pixelMap.contains((shape.position._1 + 2, shape.position._2 + 2)) ||
                  pixelMap.contains((shape.position._1 + 2, shape.position._2 + 3))
    }

    private def isNextMoveCritical(shape: Shape): Boolean = {
        shape.signature match
            case SQUARE =>
                shape.position._2 + 2 >= gridBound ||
                  pixelMap.contains((shape.position._1, shape.position._2 + 2)) ||
                  pixelMap.contains((shape.position._1 + 1, shape.position._2 + 2))
            case T =>
                shape.position._2 + 2 >= gridBound ||
                  pixelMap.contains((shape.position._1 - 1, shape.position._2 + 1)) ||
                  pixelMap.contains((shape.position._1, shape.position._2 + 2)) ||
                  pixelMap.contains((shape.position._1 + 1, shape.position._2 + 1))
            case L =>
                shape.position._2 + 3 >= gridBound ||
                  pixelMap.contains((shape.position._1, shape.position._2 + 3)) ||
                  pixelMap.contains((shape.position._1 + 1, shape.position._2 + 3))
            case REVERSE_L =>
                shape.position._2 + 3 >= gridBound ||
                  pixelMap.contains((shape.position._1, shape.position._2 + 3)) ||
                  pixelMap.contains((shape.position._1 - 1, shape.position._2 + 3))
            case BAR =>
                shape.position._2 + 3 >= gridBound ||
                  pixelMap.contains((shape.position._1, shape.position._2 + 3))
            case S =>
                shape.position._2 + 3 >= gridBound ||
                  pixelMap.contains((shape.position._1, shape.position._2 + 2)) ||
                  pixelMap.contains((shape.position._1 + 1, shape.position._2 + 3))
    }

    private def getNextShape: Shape = {
        val randCoord = (10, 0)
        val randColor = Color.rgb(Random.nextInt(256), Random.nextInt(256), Random.nextInt(256))
        val signature = if (!TEST_MODE) ShapeSignature.values(Random.nextInt(ShapeSignature.values.length)) else T

        signature match
            case SQUARE =>
                Shape (
                    randCoord,
                    signature,
                    randColor,
                    List(
                        // BL
                        Pixel((randCoord._1, randCoord._2 + 1)),
                        // TL
                        Pixel((randCoord._1, randCoord._2)),
                        // TR
                        Pixel((randCoord._1 + 1, randCoord._2)),
                        // BR
                        Pixel((randCoord._1 + 1, randCoord._2 + 1))
                    )
                )
            case T =>
                Shape(
                    randCoord,
                    signature,
                    randColor,
                    List(
                        // TM
                        Pixel((randCoord._1, randCoord._2)),
                        // TL
                        Pixel((randCoord._1 - 1, randCoord._2)),
                        // TR
                        Pixel((randCoord._1 + 1, randCoord._2)),
                        // M
                        Pixel((randCoord._1, randCoord._2 + 1)),
                    )
                )
            case L =>
                Shape(
                    randCoord,
                    signature,
                    randColor,
                    List(
                        // TM
                        Pixel((randCoord._1, randCoord._2)),
                        // M
                        Pixel((randCoord._1, randCoord._2 + 1)),
                        // BM
                        Pixel((randCoord._1, randCoord._2 + 2)),
                        // BR
                        Pixel((randCoord._1 + 1, randCoord._2 + 2)),
                    )
                )
            case REVERSE_L =>
                Shape(
                    randCoord,
                    signature,
                    randColor,
                    List(
                        // TM
                        Pixel((randCoord._1, randCoord._2)),
                        // M
                        Pixel((randCoord._1, randCoord._2 + 1)),
                        // BM
                        Pixel((randCoord._1, randCoord._2 + 2)),
                        // BL
                        Pixel((randCoord._1 - 1, randCoord._2 + 2)),
                    )
                )
            case BAR =>
                Shape(
                    randCoord,
                    signature,
                    randColor,
                    List(
                        // TM
                        Pixel((randCoord._1, randCoord._2)),
                        // M
                        Pixel((randCoord._1, randCoord._2 + 1)),
                        // BM
                        Pixel((randCoord._1, randCoord._2 + 2)),
                    )
                )
            case S =>
                Shape(
                    randCoord,
                    signature,
                    randColor,
                    List(
                        // T
                        Pixel((randCoord._1, randCoord._2)),
                        // ML
                        Pixel((randCoord._1, randCoord._2 + 1)),
                        // MR
                        Pixel((randCoord._1 + 1, randCoord._2 + 1)),
                        // B
                        Pixel((randCoord._1 + 1, randCoord._2 + 2)),
                    )
                )        
    }

    private def getGameOver(): List[Rectangle] = {
        List(
            // G
            new Rectangle {
                x = 4 * cellSize
                y = 4 * cellSize
                width = 2 * cellSize
                height = cellSize
                fill = Black
            },
            new Rectangle {
                x = 3 * cellSize
                y = 5 * cellSize
                width = cellSize
                height = 3 * cellSize
                fill = Black
            },
            new Rectangle {
                x = 4 * cellSize
                y = 8 * cellSize
                width = 2 * cellSize
                height = cellSize
                fill = Black
            },
            new Rectangle {
                x = 5 * cellSize
                y = 6 * cellSize
                width = 2 * cellSize
                height = cellSize
                fill = Black
            },
            new Rectangle {
                x = 6 * cellSize
                y = 7 * cellSize
                width = cellSize
                height = cellSize
                fill = Black
            },
            //A
            new Rectangle {
                x = 8 * cellSize
                y = 4 * cellSize
                width = cellSize
                height = 5 * cellSize
                fill = Black
            },
            new Rectangle {
                x = 9 * cellSize
                y = 4 * cellSize
                width = cellSize
                height = cellSize
                fill = Black
            },
            new Rectangle {
                x = 9 * cellSize
                y = 6 * cellSize
                width = cellSize
                height = cellSize
                fill = Black
            },
            new Rectangle {
                x = 10 * cellSize
                y = 4 * cellSize
                width = cellSize
                height = 5 * cellSize
                fill = Black
            },
            // M
            new Rectangle {
                x = 12 * cellSize
                y = 4 * cellSize
                width = cellSize
                height = 5 * cellSize
                fill = Black
            },
            new Rectangle {
                x = 13 * cellSize
                y = 5 * cellSize
                width = cellSize
                height = 2 * cellSize
                fill = Black
            },
            new Rectangle {
                x = 14 * cellSize
                y = 4 * cellSize
                width = cellSize
                height = 5 * cellSize
                fill = Black
            },
            // E
            new Rectangle {
                x = 16 * cellSize
                y = 4 * cellSize
                width = 3 * cellSize
                height = cellSize
                fill = Black
            },
            new Rectangle {
                x = 16 * cellSize
                y = 5 * cellSize
                width = cellSize
                height = 3 * cellSize
                fill = Black
            },
            new Rectangle {
                x = 16 * cellSize
                y = 8 * cellSize
                width = 3 * cellSize
                height = cellSize
                fill = Black
            },
            new Rectangle {
                x = 17 * cellSize
                y = 6 * cellSize
                width = cellSize
                height = cellSize
                fill = Black
            },
            // O
            new Rectangle {
                x = 4 * cellSize
                y = 10 * cellSize
                width = 2 * cellSize
                height = cellSize
                fill = Black
            },
            new Rectangle {
                x = 3 * cellSize
                y = 11 * cellSize
                width = cellSize
                height = 3 * cellSize
                fill = Black
            },
            new Rectangle {
                x = 6 * cellSize
                y = 11 * cellSize
                width = cellSize
                height = 3 * cellSize
                fill = Black
            },
            new Rectangle {
                x = 4 * cellSize
                y = 14 * cellSize
                width = 2 * cellSize
                height = cellSize
                fill = Black
            },
            // V
            new Rectangle {
                x = 8 * cellSize
                y = 10 * cellSize
                width = cellSize
                height = 4 * cellSize
                fill = Black
            },
            new Rectangle {
                x = 9 * cellSize
                y = 14 * cellSize
                width = cellSize
                height = cellSize
                fill = Black
            },
            new Rectangle {
                x = 10 * cellSize
                y = 10 * cellSize
                width = cellSize
                height = 4 * cellSize
                fill = Black
            },
            // E
            new Rectangle {
                x = 12 * cellSize
                y = 10 * cellSize
                width = 3 * cellSize
                height = cellSize
                fill = Black
            },
            new Rectangle {
                x = 12 * cellSize
                y = 11 * cellSize
                width = cellSize
                height = 3 * cellSize
                fill = Black
            },
            new Rectangle {
                x = 12 * cellSize
                y = 14 * cellSize
                width = 3 * cellSize
                height = cellSize
                fill = Black
            },
            new Rectangle {
                x = 13 * cellSize
                y = 12 * cellSize
                width = cellSize
                height = cellSize
                fill = Black
            },
            // R
            new Rectangle {
                x = 16 * cellSize
                y = 10 * cellSize
                width = cellSize
                height = 5 * cellSize
                fill = Black
            },
            new Rectangle {
                x = 17 * cellSize
                y = 10 * cellSize
                width = cellSize
                height = cellSize
                fill = Black
            },
            new Rectangle {
                x = 17 * cellSize
                y = 12 * cellSize
                width = cellSize
                height = cellSize
                fill = Black
            },
            new Rectangle {
                x = 18 * cellSize
                y = 11 * cellSize
                width = cellSize
                height = cellSize
                fill = Black
            },
            new Rectangle {
                x = 18 * cellSize
                y = 13 * cellSize
                width = cellSize
                height = 2 * cellSize
                fill = Black
            }
        )
    }
}
