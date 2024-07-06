package app

import scalafx.animation.Timeline.Indefinite
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.beans.property.ObjectProperty
import scalafx.scene.Scene
import scalafx.scene.paint.Color.*
import scalafx.util.Duration

object Main extends JFXApp3 {

    private val windowSize = 600
    private val cellSize = 40

    override def start(): Unit = {

        val game: ObjectProperty[Game] = ObjectProperty(Game(List(), cellSize))

        stage = new PrimaryStage {
            title = "SCALTRIS - Tetris with ScalaFX"
            width = windowSize
            height = windowSize
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
                    time = Duration(250),
                    onFinished = _ => game.update(game.value.play())
                )
            )
            cycleCount = Indefinite
        }.play()
    }
}
