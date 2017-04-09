package model

import pwo.Pwo

/**
  * Created by wojciech on 07.04.17. 
  */
class PwoXY(z: VectorXY => Double, op: (Double, Double) => Boolean) extends Pwo[VectorXY, VectorXY] {

  protected def best(p1: VectorXY, p2: VectorXY): VectorXY =
    if (op(z(p1), z(p2))) p1 else p2

  protected def update(c1: Double, c2: Double, pos: VectorXY, velocity: VectorXY, pBest: VectorXY, gBest: VectorXY): UpdateRes = {
    val p1 = math.random()
    val p2 = math.random()
    val nv = velocity + (pBest - pos) * p1 * c1 + (gBest - pos) * p2 * c2
    val np = pos + nv
    UpdateRes(np, nv)
  }




}
