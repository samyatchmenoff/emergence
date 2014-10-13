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

  def constrain(w: Int, h: Int) = {
    HexPosition(XX.constrain(0, w, x), XX.constrain(0, h, y))
  }
}

object HexPosition {
  def random = HexPosition(Random.nextInt(110), Random.nextInt(60))
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
  type Behavior = ((Float, WorldState, Agent) ⇒ (WorldState, Agent))

  def constrain(min: Float, max: Float, value: Float) =
    if (value < min) min else if (value > max) max else value
  def constrain(min: Int, max: Int, value: Int) =
    if (value < min) min else if (value > max) max else value
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
        behavior(signal, w, a)
      }
    result
  }

  def vitalize(delta: Float) = copy(vitality = constrain(-1f, 1f, vitality + delta))

  def move(d: Direction) = copy(position = (position + d).constrain(110, 60))
}

object Agent {

  def random = new Agent(HexPosition.random, 1f, sensors, behaviors, Genome(sensors.size * behaviors.size))

  val sensors: List[Sensor] =
    // related neighbor
    (for (d ← Direction.All) yield {
      (world: WorldState, agent: Agent) ⇒
        world(agent.position + d) match {
          case Some(other: Agent) ⇒ Genome.relatedness(agent.genome, other.genome)
          case _ ⇒ 0f
        }
    }) ::: List[Sensor]()
//            Direction.All.flatMap { d ⇒
//              food.get(pos + d) match {
//                case Some(f: Food) ⇒ Some(ViewFood(f.food_value, d))
//                case _ ⇒ None
//              }
//            } ++

//            food.get(pos).map(x ⇒ FoodPresent(x.food_value)).toList


  val move_cost     = 0.100f
//  val eat_gain      = 0.060f
//  val food_growth   = 0.001f
//  val round_cost    = 0.010f
  val attack_cost   = 0.300f
//  val food_rate     = 0.015f
//  val cost_of_birth = 0.500f

  val behaviors: List[Behavior] =
    (for (d ← Direction.All) yield {
      (signal: Float, world: WorldState, agent: Agent) ⇒
        if (signal < .5f)
          (world, agent)
        else {
          val new_agent = agent.vitalize(-move_cost).move(d)
          val (new_world, new_agent2)  = world.move(agent, new_agent)
          (new_world, new_agent2)
        }
    }) ::: (for (d ← Direction.All) yield {
      (signal: Float, world: WorldState, agent: Agent) ⇒
        if (signal < .5f)
          (world, agent)
        else {
          val new_agent = agent.vitalize(-attack_cost).move(d)
          val (new_world, new_agent2)  = world.kill(agent, new_agent)
          (new_world, new_agent2)
        }

//      attack adjacent
//      (signal: Float, world: WorldState, pos: HexPosition, agent: Agent) ⇒
//        val new_pos = pos + d
//        world(new_pos) match {
//          case Some(victim) ⇒
//            val new_agent = agent.vitalize(-attack_cost)
//            val new_world = world.entities - pos + (new_pos → new_agent)
//            (WorldState(new_world), new_pos, new_agent)
//          case _ ⇒ (world, pos, agent) // bump
//        }
    }) ::: List[Behavior]()

//case class Eat(å: Float) extends Signal
//case class Move(å: Float, d: Direction) extends Signal
//case class Attack(å: Float, d: Direction) extends Signal
//case class Divide(å: Float, d: Direction) extends Signal


//
//  val vitality_of_offspring = 0.001f

//                case Eat(_) ⇒ food.get(pos) match {
//                  case Some(f) ⇒
//                    food.update(pos, f.copy(food_value = f.food_value - eat_gain))
//                    (pos, agent.vitalize(min(f.food_value, eat_gain)))
//                  case _ ⇒
//                    (pos, agent) // bump
//                }

//                case Attack(_, d) ⇒ entities.get(pos + d) match {
//                  case Some(_: Agent) ⇒
//                    food.update(pos + d, Food(blood = true))
//                    (pos + d, agent.vitalize(-attack_cost))
//                  case _ ⇒
//                    (pos, agent) // bump
//                }

//                case Divide(_, d) ⇒ entities.get(pos + d) match {
//                  case None ⇒
//                    val m = mutate(agent.genome_stability, agent.genome_mobility) _
//                    entities.update(pos + d,
//                      Agent(
//                        vitality = vitality_of_offspring,
//                        m(agent.loaf),
//                        m(agent.foodie),
//                        m(agent.bloodlust),
//                        m(agent.wander),
//                        m(agent.sex_drive),
//                        m(agent.genome_stability),
//                        m(agent.genome_mobility)
//                      )
//                    )
//                    (pos + d, agent.vitalize(-cost_of_birth))
//                  case _ ⇒
//                    (pos, agent) // bump
//                }
//              }

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
    assert(g0.values.size == g1.values.size)
    (0 until g0.values.size).map(i ⇒ pow(g0.values(i) - g1.values(1), 2)).sum.toFloat
  }
}

class WorldState(
  agent_set: Set[Agent] = Set(),
  position_map: Map[HexPosition, Agent] = Map()
) {

  val agents = agent_set

  def apply(pos: HexPosition): Option[Agent] = position_map.get(pos)

  def step: WorldState = {
    val world = spawn
    agent_set.foldLeft(world) { (world: WorldState, agent: Agent) ⇒
      agent.step(world)
    }
  }

  def spawn = {
    val a = Agent.random
    this(a.position) match {
      case None ⇒ new WorldState(agent_set + a, position_map + (a.position → a))
      case _ ⇒ this
    }
  }

  def move(a0: Agent, a1: Agent): (WorldState, Agent) = {
    val old_pos = a0.position
    val new_pos = a1.position
    apply(new_pos) match {
      case None ⇒ (new WorldState((agent_set - a0) + a1, (position_map - old_pos) + (a1.position → a1)), a1)
      case _ ⇒ (this, a0)
    }
  }

  def kill(a0: Agent, a1: Agent): (WorldState, Agent) = {
    val old_pos = a0.position
    val new_pos = a1.position
    apply(new_pos) match {
      case Some(victim) ⇒ (new WorldState((agent_set - a0 - victim) + a1, (position_map - old_pos - new_pos) + (a1.position → a1)), a1)
      case _ ⇒ (this, a0)
    }
  }
  
}


class Life extends Game {

  val HEX_RADIUS = 9.0f
  val AGENT_RADIUS = 4.0f

  lazy val camera = new OrthographicCamera
  lazy val shapeRenderer = new ShapeRenderer

  var state = new WorldState()

  override def create() {
    camera.setToOrtho(false)
  }

  override def render() {

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

    for (a ← state.agents) {
      val v = a.position.vector * HEX_RADIUS
      shapeRenderer.begin(ShapeType.Filled)
      shapeRenderer.setColor(.2f, .2f, .2f, .2f)
      shapeRenderer.circle(v.x, v.y, AGENT_RADIUS)
      shapeRenderer.end()
    }
  }
}

