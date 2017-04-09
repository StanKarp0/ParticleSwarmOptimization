package view

import model.VectorXY

import scalafx.scene.shape.Circle
import scalafx.Includes._
import scalafx.animation.Timeline
import scalafx.scene.paint.Color

/**
  * Created by wojciech on 07.04.17. 
  */
abstract class Particle extends Circle {
  val timeline: Timeline
}

object Particle {

  def apply(history: List[VectorXY], scale: VectorXY => VectorXY, interval: Double): Particle = {

    new Particle() {
      history.lastOption.foreach{v =>
        val sv = scale(v)
        centerX = sv.x
        centerY = sv.y
      }
      radius = 0.7
      fill = Color.White
      val timeline = new Timeline {
        this.autoReverse = false
        keyFrames = history.reverse.map(scale).zipWithIndex.flatMap { case (v, i) => Seq(
          at((i * interval).s) {
            centerX -> v.x
          },
          at((i * interval).s) {
            centerY -> v.y
          }
        )
        }
      }
    }
  }

}
