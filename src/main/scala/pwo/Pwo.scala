package pwo

/**
  * Created by wojciech on 07.04.17. 
  */



abstract class Pwo[P, V]{

  case class Particle(history: List[P], velocity: V, pBest: P)
  case class State(particles: List[Particle], gBest: P)
  case class UpdateRes(position: P, velocity: V)

  protected def best(p1: P, p2: P): P
  protected def update(c1: Double, c2: Double, pos: P, velocity: V, pBest: P, gBest: P): UpdateRes

  def start(x: List[(P, V)], c1: Double, c2: Double): Option[Iterator[State]] = {
    val particles = x.map(e => Particle(List(e._1), e._2, e._1))
    x.headOption.map(head => Iterator.iterate(State(particles, head._1)){ state =>
      val gBest = best(state.particles.map(_.pBest).reduce(best), state.gBest)
      State(state.particles.collect{
        case p@ Particle(first :: _, v, pBest) =>
          val result = update(c1, c2, first, v, pBest, gBest)
          Particle(result.position :: p.history, result.velocity, best(result.position, pBest))
      }, gBest)
    })
  }

}
