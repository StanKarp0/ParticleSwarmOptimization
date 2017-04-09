package model

import scala.util.Random

/**
  * Created by wojciech on 07.04.17. 
  */
case class VectorXY(x: Double, y: Double) {
  def -(v: VectorXY): VectorXY = VectorXY(x - v.x, y - v.y)
  def +(v: VectorXY): VectorXY = VectorXY(v.x + x, v.y + y)
  def *(v: Double): VectorXY = VectorXY(x * v, y * v)
  def +(v: Double): VectorXY = VectorXY(x + v, y + v)
  def len : Double = math.sqrt(x * x + y * y)
}

object VectorXY {

  def randomRange(xMin: Double, xMax: Double, yMin: Double, yMax: Double): Iterator[VectorXY] =
    Iterator.continually(VectorXY(
      Random.nextDouble() * (xMax - xMin) + xMin,
      Random.nextDouble() * (yMax - yMin) + yMin
    ))

}
