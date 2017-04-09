package view

import model._

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.Includes._

/**
  * Created by wojciech on 07.04.17. 
  */
object Main extends JFXApp {

  val function: (VectorXY) => Double = (v: VectorXY) =>
    v.x * v.x + v.y * v.y - 20 * (math.cos(math.Pi * v.x) + math.cos(math.Pi * v.y) - 2)

  val function2: (VectorXY) => Double = (v: VectorXY) =>
    math.pow(v.x + 4, 2) + math.pow(v.y - 4, 2)

  def borderMod(x: Double): Double =
    (if(math.floor((math.abs(x)+10)/20)%2 == 1) {
      10 - math.abs(x+10) % 20
    } else {
      math.abs(x+10) % 20 - 10
    })* (if(x< -10) -1 else 1)

  val borderM = Border.square(-15, 10)


  val clerc = new PwoXY(function, _ < _) with Clerc

  val clercBorder = new PwoXY(function, _ < _) with Clerc with Border {
    override val border: (VectorXY) => VectorXY = (v: VectorXY) =>
      VectorXY(borderMod(v.x), borderMod(v.y))
  }

  val psoBorder = new PwoXY(function, _ < _) with MaxV with Border {
    override val maxV: Double = 10
    override val border: (VectorXY) => VectorXY = (v: VectorXY) =>
      VectorXY(borderM(v.x), borderM(v.y))
  }

  val xVectors: List[(VectorXY, VectorXY)] = VectorXY.randomRange(-10, 10, -10, 10)
    .zip(Iterator.continually(VectorXY(0,0))).take(1000).toList

  stage = new PrimaryStage {

    scene = new Scene(800, 600) {

      root = new BorderPane() {
        style = "-fx-background-color: black;"
        psoBorder.start(xVectors, 2.03, 2.03).foreach{iterator =>
          val particles = iterator.drop(100).next.particles.map{ p =>
            Particle(p.history, v => v * 20 + VectorXY(600, 350), 0.5)
          }
          center = new Pane {
            children = particles
          }

          top = new Button("Start") {
            onAction = (_: ActionEvent) => {
              particles.foreach{p =>
                p.timeline.play()
              }
            }
          }

        }

      }

    }

  }

}
