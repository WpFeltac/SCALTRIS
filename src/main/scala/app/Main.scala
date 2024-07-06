package app

import app.Direction.{LEFT, NONE, RIGHT}
import app.ShapeSignature.{SQUARE, T}
import javafx.scene.input.KeyCode
import javafx.scene.input.KeyEvent
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

        val direction: ObjectProperty[Direction] = ObjectProperty(NONE)
        val game: ObjectProperty[Game] = ObjectProperty(Game(ShapeSignature.values(Random.nextInt(ShapeSignature.values.length)), List(), Map(), cellSize, gridBound))

        stage = new PrimaryStage {
            title = "SCALTRIS - Tetris with ScalaFX"
            width = windowSize + 45
            height = windowSize + cellSize + 60
            scene = new Scene {
                fill = White
                content = game.value.draw()
                game.onChange {
                    content = game.value.draw()
                }
                onKeyPressed = moveShapeHorizontally(direction, _)
                onKeyReleased = resetDirection(direction, _)
            }
        }

        new Timeline {
            keyFrames = List(
                KeyFrame(
                    time = Duration(200),
                    onFinished = _ => game.update(game.value.play(direction.value))
                )
            )
            cycleCount = Indefinite
        }.play()
    }

    private def moveShapeHorizontally(direction: ObjectProperty[Direction], key: KeyEvent): Unit = {
        key.getCode match {
            case KeyCode.UP => ()
            case KeyCode.DOWN  => ()
            case KeyCode.LEFT  => direction.update(LEFT)
            case KeyCode.RIGHT => direction.update(RIGHT)
            case _     => ()
        }
    }

    private def resetDirection(direction: ObjectProperty[Direction], key: KeyEvent): Unit = {
        direction.update(NONE)
    }
}
