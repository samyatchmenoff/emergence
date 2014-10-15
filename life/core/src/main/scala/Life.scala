package life

import scala.math._
import scala.util.Random
import com.badlogic.gdx.Gdx
import com.badlogic.gdx.Game
import com.badlogic.gdx.graphics.GL20
import com.badlogic.gdx.graphics.OrthographicCamera
import com.badlogic.gdx.graphics.glutils.ShapeRenderer
import com.badlogic.gdx.graphics.glutils.ShapeRenderer.ShapeType
import life.math._

case class HexPosition(x: Int, y: Int) {
  def vector = Vector2(1.5f * x, y * sqrt(3).toFloat + (x % 2) * .5f * sqrt(3).toFloat)

  def +(d: Direction): HexPosition = {
    d match {
      case Direction.RightUp => HexPosition(x + 1, y + (x%2))
      case Direction.Up => HexPosition(x, y + 1)
      case Direction.LeftUp => HexPosition(x - 1, y + (x%2))
      case Direction.LeftDown => HexPosition(x - 1, y - ((x+1)%2))
      case Direction.Down => HexPosition(x, y - 1)
      case Direction.RightDown => HexPosition(x + 1, y - ((x+1)%2))
    }
  }

  def constrain(x0: Int, y0: Int, w: Int, h: Int) = {
    HexPosition(XX.constrain(x0, w, x), XX.constrain(y0, h, y))
  }
}

object HexPosition {
  def random = HexPosition(10 + Random.nextInt(10), 10 + Random.nextInt(10))
}

sealed trait Direction
object Direction {
  case object RightUp extends Direction
  case object Up extends Direction
  case object LeftUp extends Direction
  case object LeftDown extends Direction
  case object Down extends Direction
  case object RightDown extends Direction
  val All = List(RightUp, Up, LeftUp, LeftDown, Down, RightDown)
  def Rnd = All(Random.nextInt(6))
}


object XX {
  type Sensor = ((WorldState, Agent) ⇒ Float)
  type Behavior = ((WorldState, Agent) ⇒ (WorldState, Agent))

  def constrain(min: Float, max: Float, value: Float) =
    if (value < min) min else if (value > max) max else value
  def constrain(min: Int, max: Int, value: Int) =
    if (value < min) min else if (value > max) max else value

  val HEX_RADIUS = 9.0f
}
import XX._


case class Agent(
  position: HexPosition,
  vitality: Float,
  sensors: List[Sensor],
  behaviors: List[Behavior],
  genome: Genome
) {

  def step(worldState: WorldState): WorldState = {
    var i = 0
    val (result, _) = behaviors.foldLeft((worldState, this)) {
      case ((w: WorldState, a: Agent), behavior: Behavior) ⇒
        val sum_of_weighted_inputs = sensors.map { sensor: Sensor ⇒
          val out = sensor(w, a) * genome.values(i)
          i += 1
          out
        }.sum
        val signal = (1d / (1d + pow(E, -sum_of_weighted_inputs))).toFloat
        val signal_weight = genome.values(i)
        i += 1
        val signal_threshold = genome.values(i)
        i += 1
        if (signal * signal_weight < signal_threshold) {
          (w, a)
        } else {
          behavior(w, a)
        }
      }
    result
  }

  def vitalize(delta: Float) = copy(vitality = constrain(0f, 1f, vitality + delta))

  def move(d: Direction) = copy(position = (position + d).constrain(3, 3, 100, 50))

  val timestamp = System.currentTimeMillis
}

object Agent {

  def random = new Agent(HexPosition.random, 1f, sensors, behaviors, Genome(sensors.size * behaviors.size + behaviors.size + behaviors.size))

  val sensors: List[Sensor] =
    // related neighbor
    (for (d ← Direction.All) yield {
      (world: WorldState, agent: Agent) ⇒
        world(agent.position + d) match {
          case Some(other: Agent) ⇒ Genome.relatedness(agent.genome, other.genome)
          case _ ⇒ 0f
        }
    }) ::: List[Sensor]()

  val move_cost     = 0.001f
  val attack_gain   = 0.100f
  val cost_of_birth = 0.200f
  val baby_vitality = 0.100f

  val behaviors: List[Behavior] =
    (for (d ← Direction.All) yield {
      (world: WorldState, agent: Agent) ⇒
        world.move(agent, agent.vitalize(-move_cost).move(d))
    }) ::: (for (d ← Direction.All) yield {
      (world: WorldState, agent: Agent) ⇒
        world.kill(agent, agent.vitalize(attack_gain).move(d))
    }) ::: (for (d ← Direction.All) yield {
      (world: WorldState, agent: Agent) ⇒
        if (agent.vitality > cost_of_birth ) {
          val bud_agent = agent.copy(position = agent.position + d, vitality = baby_vitality, genome = agent.genome.mutate(.9f, .1f))
          val new_world = world.add_agent(bud_agent)
          new_world.update_agent(agent, agent.vitalize(-cost_of_birth))
        } else (world, agent)
     }) ::: List[Behavior]()

}

case class Genome(values: List[Float]) {

  def mutate(genome_stability: Float, genome_mobility: Float): Genome = {
    val mutated_values = values.map { parent_gene ⇒
      if (Random.nextFloat > genome_stability)
        max(-1f, min(1f, parent_gene + (Random.nextFloat - .5f) * genome_mobility))
      else
        parent_gene
    }
    Genome(mutated_values)
  }

}

object Genome {
  def apply(size: Int) = new Genome(List.fill(size)(Random.nextFloat))
  def relatedness(g0: Genome, g1: Genome): Float = {
//    assert(g0.values.size == g1.values.size)
    (0 until g0.values.size).map(i ⇒ pow(g0.values(i) - g1.values(1), 2)).sum.toFloat
  }
}

class WorldState(
  val position_map: Map[HexPosition, Agent] = Map()
) {

  def apply(pos: HexPosition): Option[Agent] = position_map.get(pos)

  def step: WorldState = {
    val a = position_map.values.minBy(_.timestamp)
    if (a.vitality > 0f) {
      a.step(this.update_agent(a, a.vitalize(-.2f))._1)
    } else {
      Annotation.all ::= DieoffAnnotation(a.position)
      remove_agent(a)
    }
  }

  def spawn = {
    add_agent(Agent.random)
  }

  def move(a0: Agent, a1: Agent): (WorldState, Agent) = {
    val old_pos = a0.position
    val new_pos = a1.position
    apply(new_pos) match {
      case None ⇒
        Annotation.all ::= MoveAnnotation(a1, old_pos, new_pos)
        (new WorldState((position_map - old_pos) + (a1.position → a1)), a1)
      case _ ⇒ (this, a0)
    }
  }

  def kill(a0: Agent, a1: Agent): (WorldState, Agent) = {
    val old_pos = a0.position
    val new_pos = a1.position
    apply(new_pos) match {
      case Some(victim) ⇒
        Annotation.all ::= MoveAnnotation(a1, old_pos, new_pos)
        Annotation.all ::= MurderAnnotation(new_pos)
        (new WorldState((position_map - old_pos - new_pos) + (a1.position → a1)), a1)
      case _ ⇒ (this, a0)
    }
  }

  def add_agent(a: Agent): WorldState = {
    apply(a.position) match {
      case None ⇒
        Annotation.all ::= BirthAnnotation(a.position)
        new WorldState(position_map + (a.position → a))
      case _ ⇒ this
    }
  }

  def update_agent(a0: Agent, a1: Agent): (WorldState, Agent) = {
    val old_pos = a0.position
    val new_pos = a1.position
    (new WorldState((position_map - old_pos) + (new_pos → a1)), a1)
  }

  def remove_agent(a: Agent): WorldState = {
    new WorldState(position_map - a.position)
  }
  
}


class Life extends Game {

  lazy val camera = new OrthographicCamera
  lazy val shapeRenderer = new ShapeRenderer

  var state = new WorldState()

  override def create() {
    camera.setToOrtho(false)
  }

  var i = 0
  override def render() {
    if (state.position_map.isEmpty) state = state.spawn
    if (i % 100 == 0) state = state.spawn
    i += 1
    state = state.step

    Gdx.gl20.glViewport(0, 0, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    Gdx.gl20.glClearColor(0.2f, 0.15f, 0.1f, 1)
    Gdx.gl20.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)

    camera.update()
    shapeRenderer.setProjectionMatrix(camera.combined)

    val hexagonPoints = for(i <- 0 to 6) yield {
      (HEX_RADIUS * cos(toRadians(i * 60.0)), HEX_RADIUS * sin(toRadians(i * 60.0)))
    }
    val hexagonTriangles: Seq[(Float, Float, Float, Float, Float, Float)] =
      (hexagonPoints sliding 2 map { case Seq((x0: Double, y0: Double), (x1: Double, y1: Double)) =>
        (x0.toFloat, y0.toFloat, x1.toFloat, y1.toFloat, 0.0f, 0.0f): (Float, Float, Float, Float, Float, Float)
      }).toSeq

    shapeRenderer.begin(ShapeType.Filled)
    for(x <- 0 to 128; y <- 0 to 72) {
      val (r,g,b) = (x%2) + 2 * (y%2) match {
        case 0 => (0.494f, 0.937f, 0.525f)
        case 1 => (0.337f, 0.898f, 0.376f)
        case 2 => (0.208f, 0.855f, 0.251f)
        case _ => (0.071f, 0.804f, 0.118f)
      }
      shapeRenderer.setColor(r, g, b, 1.0f)
      val v = HexPosition(x, y).vector * HEX_RADIUS
      for((x0,y0,x1,y1,x2,y2) <- hexagonTriangles) {
        shapeRenderer.triangle(x0 + v.x, y0 + v.y, x1 + v.x, y1 + v.y, x2 + v.x, y2 + v.y)
      }
    }
    shapeRenderer.end()

    for (a ← state.position_map.values) {
      val v = a.position.vector * HEX_RADIUS
      shapeRenderer.begin(ShapeType.Filled)
      shapeRenderer.setColor(a.genome.values(0), a.genome.values(1), a.genome.values(2), 1f)
      shapeRenderer.circle(v.x, v.y, 1 + (a.vitality + 4))
      shapeRenderer.end()
    }

    val n = System.currentTimeMillis
    Annotation.all = for (a ← Annotation.all; if n - a.timestamp < 5000) yield {
      a.draw(shapeRenderer)
      a
    }
  }
}

object Annotation {
  var all = List[Annotation]()
}

trait Annotation {
  val timestamp = System.currentTimeMillis
  def draw(shapeRenderer: ShapeRenderer): Unit
}

case class MoveAnnotation(a: Agent, from: HexPosition, to: HexPosition) extends Annotation {
  val fromv = from.vector * HEX_RADIUS
  val tov = to.vector * HEX_RADIUS
  def draw(shapeRenderer: ShapeRenderer) = {
    shapeRenderer.begin(ShapeType.Line)
    shapeRenderer.setColor(a.genome.values(0), a.genome.values(1), a.genome.values(2), 1f)
    shapeRenderer.line(fromv.x, fromv.y, tov.x, tov.y)
    shapeRenderer.end()
  }
}

case class MurderAnnotation(pos: HexPosition) extends Annotation {
  val v = pos.vector * HEX_RADIUS
  def draw(shapeRenderer: ShapeRenderer) = {
    shapeRenderer.begin(ShapeType.Filled)
    shapeRenderer.setColor(1, 0, 0, 1)
    shapeRenderer.rect(v.x, v.y, 5, 5)
    shapeRenderer.end()
  }
}

case class BirthAnnotation(pos: HexPosition) extends Annotation {
  val v = pos.vector * HEX_RADIUS
  def draw(shapeRenderer: ShapeRenderer) = {
    shapeRenderer.begin(ShapeType.Filled)
    shapeRenderer.setColor(0, 0, 1, 1)
    shapeRenderer.rect(v.x - 4, v.y - 4, 5, 5)
    shapeRenderer.end()
  }
}

case class DieoffAnnotation(pos: HexPosition) extends Annotation {
  val v = pos.vector * HEX_RADIUS
  def draw(shapeRenderer: ShapeRenderer) = {
    shapeRenderer.begin(ShapeType.Filled)
    shapeRenderer.setColor(.95f, 1f, 1f, 1)
    shapeRenderer.rect(v.x - 2, v.y - 2, 4, 4)
    shapeRenderer.end()
  }
}

