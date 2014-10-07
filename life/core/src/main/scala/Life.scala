package life

import scala.collection.mutable
import scala.math._
import scala.util.Random
import com.badlogic.gdx.Gdx
import com.badlogic.gdx.Game
import com.badlogic.gdx.graphics.GL20
import com.badlogic.gdx.graphics.OrthographicCamera
import com.badlogic.gdx.graphics.glutils.ShapeRenderer
import com.badlogic.gdx.graphics.glutils.ShapeRenderer.ShapeType
import com.badlogic.gdx.Input.Keys
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
}

object HexPosition {
  def random = HexPosition(1 + Random.nextInt(79), 1 + Random.nextInt(39))
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

sealed trait Entity

case class Agent(
  // physical model
  vitality: Float = 1.0f,

  // genetic model
  loaf: Float = Random.nextFloat, // desire to rest
  foodie: Float = Random.nextFloat, // strength of signal to follow food
  bloodlust: Float = Random.nextFloat, // strength of attack signal
  wander: Float = Random.nextFloat, // strength of random movement signals
  sex_drive: Float = Random.nextFloat, // strength of random clone signals
  genome_stability: Float = Random.nextFloat, //
  genome_mobility: Float = Random.nextFloat //

) extends Entity {

  def decide(input: AgentInput) = AgentOutput(
    Rest(loaf) ::
    Divide(sex_drive, Direction.Rnd) ::
    Move(wander, Direction.Rnd) ::
    input.signals.map {
      case FoodPresent(å) ⇒ Eat(å * foodie)
      case ViewFood(å, d) ⇒ Move(å * foodie, d)
      case ViewAgent(å, d) ⇒ Attack((1 - å) * bloodlust, d)
    } ::: List[Signal]()
  )

  def vitalize(delta: Float) = copy(
    vitality = min(1f, max(0f, vitality + delta))
  )
  
  def relatedness(other: Agent): Float = {
    1 - (
      (abs(loaf - other.loaf) +
      abs(foodie - other.foodie) +
      abs(bloodlust - other.bloodlust) +
      abs(wander - other.wander) +
      abs(sex_drive - other.sex_drive) +
      abs(genome_stability - other.genome_stability) +
      abs(genome_mobility - other.genome_mobility)) /
      7
    )
  }
}

case class Food(food_value: Float = Random.nextFloat, blood: Boolean = false) extends Entity

case object Blocked extends Entity

sealed trait Signal {
  val å: Float
}

// input signals
case class ViewFood(å: Float, d: Direction) extends Signal
case class ViewAgent(å: Float, d: Direction) extends Signal
case class FoodPresent(å: Float) extends Signal

// output signals
case class Rest(å: Float) extends Signal
case class Eat(å: Float) extends Signal
case class Move(å: Float, d: Direction) extends Signal
case class Attack(å: Float, d: Direction) extends Signal
case class Divide(å: Float, d: Direction) extends Signal

case class AgentInput(signals: List[Signal])
case class AgentOutput(signals: List[Signal])

class WorldState(
  val entities: mutable.HashMap[HexPosition, Entity],
  var food: mutable.HashMap[HexPosition, Food]
) {

  val move_cost     = 0.100f
  val eat_gain      = 0.060f
  val food_growth   = 0.001f
  val round_cost    = 0.010f
  val attack_cost   = 0.300f
  val food_rate     = 0.015f
  val cost_of_birth = 0.500f

  val vitality_of_offspring = 0.001f
  
  def step() {

    for (pos ← entities.keys) {
      entities.get(pos) match {
        case Some(a: Agent) if a.vitality == 0f ⇒
          entities.remove(pos)
        case Some(agent: Agent) ⇒ {

          // sensor model
          val input = AgentInput(
            Direction.All.flatMap { d ⇒
              entities.get(pos + d) match {
                case Some(a: Agent) ⇒ Some(ViewAgent(agent.relatedness(a), d))
                case _ ⇒ None
              }
            } ++
            Direction.All.flatMap { d ⇒
              food.get(pos + d) match {
                case Some(f: Food) ⇒ Some(ViewFood(f.food_value, d))
                case _ ⇒ None
              }
            } ++
            food.get(pos).map(x ⇒ FoodPresent(x.food_value)).toList
          )

          val output = agent.decide(input)

          val (newPos, newAgent) = output match {
            case AgentOutput(signals) ⇒
              signals.maxBy(_.å) match {
                case Rest(_) ⇒
                  (pos, agent)
                case Move(_, d) ⇒ entities.get(pos + d) match {
                  case None ⇒
                    (pos + d, agent.vitalize(-move_cost))
                  case _ ⇒
                    (pos, agent) // bump
                }
                case Eat(_) ⇒ food.get(pos) match {
                  case Some(f) ⇒
                    food.update(pos, f.copy(food_value = f.food_value - eat_gain))
                    (pos, agent.vitalize(min(f.food_value, eat_gain)))
                  case _ ⇒
                    (pos, agent) // bump
                }
                case Attack(_, d) ⇒ entities.get(pos + d) match {
                  case Some(_: Agent) ⇒
                    food.update(pos + d, Food(blood = true))
                    (pos + d, agent.vitalize(-attack_cost))
                  case _ ⇒
                    (pos, agent) // bump
                }
                case Divide(_, d) ⇒ entities.get(pos + d) match {
                  case None ⇒
                    val m = mutate(agent.genome_stability, agent.genome_mobility) _
                    entities.update(pos + d,
                      Agent(
                        vitality = vitality_of_offspring,
                        m(agent.loaf),
                        m(agent.foodie),
                        m(agent.bloodlust),
                        m(agent.wander),
                        m(agent.sex_drive),
                        m(agent.genome_stability),
                        m(agent.genome_mobility)
                      )
                    )
                    (pos + d, agent.vitalize(-cost_of_birth))
                  case _ ⇒
                    (pos, agent) // bump
                }
              }
          }

          entities.remove(pos)
          entities.update(newPos, newAgent.vitalize(-round_cost))
        }
        case _ ⇒
      }
    }

    food = food.flatMap { case (p, f) ⇒
      if (f.food_value < 0f)
        None
      if (f.food_value < 1f)
        Some(p → f.copy(food_value = f.food_value + food_growth))
      else
        Some(p → f)
    }

    if (Random.nextFloat < food_rate)
      food.update(HexPosition.random, Food())
    if (entities.collectFirst { case x: Agent ⇒ x }.isEmpty) {
      entities.update(HexPosition.random, Agent())
    }
  }


  def mutate(genome_stability: Float, genome_mobility: Float)(parent_gene: Float): Float = {
    if (Random.nextFloat > genome_stability) {
      parent_gene + max(0f, min(1f, (Random.nextFloat - .5f) * genome_mobility))
    } else {
      parent_gene
    }
  }
}


class Life extends Game {
  val HEX_RADIUS = 9.0f
  val AGENT_RADIUS = 4.0f
  val FOOD_RADIUS = 6.0f
  val BLOCK_RADIUS = 5.0f

  lazy val camera = new OrthographicCamera
  lazy val shapeRenderer = new ShapeRenderer

  var state = new WorldState(
    entities = mutable.HashMap(
      (1 to 80).flatMap(x ⇒ Seq(HexPosition(x, 0) → Blocked, HexPosition(x, 40) → Blocked)) ++
      (1 to 40).flatMap(y ⇒ Seq(HexPosition(0, y) → Blocked, HexPosition(80, y) → Blocked))
    :_*),
    food = mutable.HashMap[HexPosition, Food]()
  )

  override def create() {
    camera.setToOrtho(false)
  }

  override def render() {
    state.step()

    Gdx.gl20.glViewport(0, 0, Gdx.graphics.getWidth(), Gdx.graphics.getHeight())
    Gdx.gl20.glClearColor(0.2f, 0.15f, 0.1f, 1)
    Gdx.gl20.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)

    camera.update()
    shapeRenderer.setProjectionMatrix(camera.combined)

    val hexagonPoints = for(i <- 0 to 6) yield {
      (HEX_RADIUS * cos(toRadians(i * 60.0)), HEX_RADIUS * sin(toRadians(i * 60.0)))
    }
    val hexagonTriangles: Seq[(Float, Float, Float, Float, Float, Float)] =
      (hexagonPoints sliding(2) map { case Seq((x0, y0), (x1, y1)) =>
        (x0.toFloat, y0.toFloat, x1.toFloat, y1.toFloat, 0.0f, 0.0f): (Float, Float, Float, Float, Float, Float)
      }).toSeq

    shapeRenderer.begin(ShapeType.Filled)
    for(x <- 0 to 128; y <- 0 to 72) {
      val (r,g,b) = ((x%2) + 2 * (y%2)) match {
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




    for ((p, entity) ← state.entities) {
      entity match {
        case agent: Agent ⇒
          val v = p.vector * HEX_RADIUS
          shapeRenderer.begin(ShapeType.Filled)
          shapeRenderer.setColor(1.0f, 0.592f, 0.243f, 1)
          shapeRenderer.circle(v.x, v.y, AGENT_RADIUS)
          shapeRenderer.end()
        case _ ⇒
          val v = p.vector * HEX_RADIUS
          shapeRenderer.begin(ShapeType.Filled)
          shapeRenderer.setColor(0f, 0f, 0f, 1)
          shapeRenderer.circle(v.x, v.y, 2)
          shapeRenderer.end()
      }
    }

    for ((p, food) ← state.food) {
        val v = p.vector * HEX_RADIUS
        shapeRenderer.begin(ShapeType.Filled)
        if (!food.blood)
          shapeRenderer.setColor(0.310f, 0.788f, 0.812f, 1)
        else
          shapeRenderer.setColor(0.750f, 0.400f, 0.400f, 1)
        shapeRenderer.circle(v.x, v.y, FOOD_RADIUS)
        shapeRenderer.end()
    }
  }
}

