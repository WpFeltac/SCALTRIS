package app

import app.ShapeSignature.SQUARE
import scalafx.animation.Timeline.Indefinite
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.beans.property.ObjectProperty
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.*
import scalafx.util.Duration

import scala.util.Random

object Main extends JFXApp3 {

    private val windowSize = 600
    private val cellSize = 30

    private val gridBound = windowSize / cellSize

    override def start(): Unit = {

        val shapeList = List.fill(1) {
            Shape(Coord(Random.nextInt(gridBound), 0), SQUARE, Color.rgb(Random.nextInt(256), Random.nextInt(256), Random.nextInt(256)))
        }

        val game: ObjectProperty[Game] = ObjectProperty(Game(shapeList, Map(), cellSize, gridBound))

        stage = new PrimaryStage {
            title = "SCALTRIS - Tetris with ScalaFX"
            width = windowSize
            height = windowSize + cellSize + 60
            scene = new Scene {
                fill = White
                content = game.value.draw()
                game.onChange {
                    content = game.value.draw()
                }
            }
        }

        new Timeline {
            keyFrames = List(
                KeyFrame(
                    time = Duration(500),
                    onFinished = _ => game.update(game.value.play())
                )
            )
            cycleCount = Indefinite
        }.play()
    }
}
