package app

import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color

import scala.util.Random

final case class Game(shapeList: List[Shape], cellSize: Int) {
    def draw(): List[Rectangle] = {
        val drawList = shapeList.map(s =>
            new Rectangle {
                x = s.position.x * cellSize
                y = s.position.y * cellSize
                width = cellSize
                height = cellSize
                fill = Color.rgb(Random.nextInt(256), Random.nextInt(256), Random.nextInt(256))
            }
        )
        
        drawList
    }
    
    def play(): Game = {
        copy()
    }
}
