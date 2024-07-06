package app

import app.Main.gridBound
import app.ShapeSignature.{DOT, SQUARE}
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.*

import scala.util.Random

final case class Game(shapeList: List[Shape], shapeMap: Map[(Int, Int), Boolean], cellSize: Int, gridBound: Int) {
    def draw(): List[Rectangle] = {
        val drawList = shapeList.map(s =>
            s.signature match
                case DOT =>
                    List(
                        new Rectangle {
                            x = s.position.x * cellSize
                            y = s.position.y * cellSize
                            width = cellSize
                            height = cellSize
                            fill = s.color
                        }
                    )
                case SQUARE =>
                    List(
                        // TL
                        new Rectangle {
                            x = s.position.x * cellSize
                            y = s.position.y * cellSize
                            width = cellSize
                            height = cellSize
                            fill = s.color
                        },
                        // TR
                        new Rectangle {
                            x = s.position.x * cellSize + cellSize
                            y = s.position.y * cellSize
                            width = cellSize
                            height = cellSize
                            fill = s.color
                        },
                        //BR
                        new Rectangle {
                            x = s.position.x * cellSize + cellSize
                            y = s.position.y * cellSize + cellSize
                            width = cellSize
                            height = cellSize
                            fill = s.color
                        },
                        // BL
                        new Rectangle {
                            x = s.position.x * cellSize
                            y = s.position.y * cellSize + cellSize
                            width = cellSize
                            height = cellSize
                            fill = s.color
                        }

                    )
        )

        // Ajout d'un rectangle noir pour la "base"
        drawList.flatten :+ Rectangle(0, 600, 600, 60)
    }

    def play(): Game = {
        val movingShape = shapeList.last

        val isNextMoveBlocked = movingShape.signature match
            case DOT => movingShape.position.y + cellSize >= gridBound
            case SQUARE =>
                movingShape.position.y + 2 >= gridBound ||
                  shapeMap.get((movingShape.position.x, movingShape.position.y + 2)).contains(true) ||
                  shapeMap.get((movingShape.position.x + 1, movingShape.position.y + 2)).contains(true)

        // Moving shape will stop and we generate a new falling one
        if(isNextMoveBlocked) {
            copy(
                shapeList = shapeList :+ Shape(Coord(Random.nextInt(gridBound), 0), SQUARE, Color.rgb(Random.nextInt(256), Random.nextInt(256), Random.nextInt(256))),
                // Registering top shape vertices for collision detection
                shapeMap = movingShape.signature match
                    case DOT => shapeMap.updated((movingShape.position.x, movingShape.position.y), true)
                    case SQUARE => shapeMap.updated((movingShape.position.x, movingShape.position.y), true)
                      .updated((movingShape.position.x + 1, movingShape.position.y), true)
            )
        }
        else {
            copy(shapeList = shapeList.filterNot(s => s == movingShape) :+ Shape(Coord(movingShape.position.x, movingShape.position.y + 1), movingShape.signature, movingShape.color))
        }
    }
}
