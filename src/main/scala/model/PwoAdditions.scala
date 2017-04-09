package model

/**
  * Created by wojciech on 08.04.17. 
  */
trait MaxV extends PwoXY {
  val maxV: Double

  abstract override protected def update(c1: Double, c2: Double, pos: VectorXY, velocity: VectorXY, pBest: VectorXY, gBest: VectorXY): UpdateRes = {
    val sup = super.update(c1, c2, pos, velocity, pBest, gBest)
    val np = pos + sup.velocity * (maxV / sup.velocity.len)
    UpdateRes(np, sup.velocity)
  }
}

trait Border extends PwoXY {
  val border: VectorXY => VectorXY

  abstract override protected def update(c1: Double, c2: Double, pos: VectorXY, velocity: VectorXY, pBest: VectorXY, gBest: VectorXY): UpdateRes = {
    val sup = super.update(c1, c2, pos, velocity, pBest, gBest)
    UpdateRes(border(sup.position), sup.velocity)
  }
}
object Border {
  def square(lower: Double, upper: Double): Double => Double = {
    val diff = upper - lower
    (x: Double) =>
      (if (math.floor((math.abs(x) + upper) / diff) % 2 == 1) {
        upper - math.abs(x + upper) % diff
      } else {
        math.abs(x + upper) % diff - upper
      }) * (if (x < lower) -1 else 1)
  }
}

trait Clerc extends PwoXY {
  abstract override protected def update(c1: Double, c2: Double, pos: VectorXY, velocity: VectorXY, pBest: VectorXY, gBest: VectorXY): UpdateRes = {
    val sup = super.update(c1, c2, pos, velocity, pBest, gBest)
    val phi = c1 + c2
    val w = 2.0 / math.abs(2 - phi - math.sqrt(phi * (phi - 4)))
    val nv = sup.velocity * w
    val np = pos + nv
    UpdateRes(np, nv)
  }
}