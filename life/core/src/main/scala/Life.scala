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
  def vector: Vector2 = {
    Vector2((1.5f * x), (y * sqrt(3).toFloat + (x%2) * 0.5f * sqrt(3).toFloat))
  }

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
  def random = HexPosition(Random.nextInt(93), Random.nextInt(45))
}

sealed trait Direction
object Direction {
  case object RightUp extends Direction
  case object Up extends Direction
  case object LeftUp extends Direction
  case object LeftDown extends Direction
  case object Down extends Direction
  case object RightDown extends Direction
  val All: List[Direction] = List(RightUp, Up, LeftUp, LeftDown, Down, RightDown)
}

sealed trait Entity

case class Agent(
  // physical model
  vitality: Float = 1.0f,

  // genetic model
  loaf: Float = .1f, // desire to rest
  foodie: Float = .1f, // strength of signal to follow food
  bloodlust: Float = .1f, // strength of attack signal
  wander: Float = .1f, // strength of random movement signals
  sex_drive: Float = .1f, // strength of random clone signals
  genome_stability: Float = 0.9f, //
  genome_mobility: Float = 0.1f //
) extends Entity {
  def decide(input: AgentInput): AgentOutput = {
    val signals =
    Rest(Random.nextFloat * loaf) ::
    input.signals.flatMap {
      case ViewEmpty(å, d) ⇒
        Seq(
          Move(Random.nextFloat * wander, d),
          Divide(Random.nextFloat * sex_drive * vitality, d)
        )
      case ViewFood(å, d) ⇒
        Seq(Move(å * foodie, d))
      case ViewAgent(å, d) ⇒
        Seq(Attack((1 - å) * bloodlust, d))
    } ::: List[Signal]()
    AgentOutput(signals)
  }
  def vitalize(delta: Float) = copy(
    vitality = min(1f, max(0f, vitality + delta))
  )
  def divide: (Agent, Agent) = {
    def mutate(parent_gene: Float): Float = {
      if (Random.nextFloat > genome_stability) {
        parent_gene + (Random.nextFloat - .5f) * genome_mobility
      } else {
        parent_gene
      }
    }
    (
      // child
      Agent(
        vitality = .3f,
        mutate(loaf),
        mutate(foodie),
        mutate(bloodlust),
        mutate(wander),
        mutate(sex_drive),
        mutate(genome_stability),
        mutate(genome_mobility)
      ),
      // mom/dad
      copy(vitality * .7f)
    )

  }

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

object Food extends Entity
object Block extends Entity

sealed trait Signal {
  val å: Float
}

// input signals
case class ViewEmpty(å: Float, d: Direction) extends Signal
case class ViewFood(å: Float, d: Direction) extends Signal
case class ViewAgent(å: Float, d: Direction) extends Signal

// output signals
case class Rest(å: Float) extends Signal
case class Move(å: Float, d: Direction) extends Signal
case class Attack(å: Float, d: Direction) extends Signal
case class Divide(å: Float, d: Direction) extends Signal

case class AgentInput(signals: List[Signal])
case class AgentOutput(signals: List[Signal])

class WorldState(
  var entities: mutable.HashMap[HexPosition,Entity]
) {

  def step(): Unit = {

    for(pos <- entities.keys) {
      entities.get(pos) match {
        case Some(a: Agent) if a.vitality < 0f ⇒ {
          entities.remove(pos)
        }
        case Some(agent: Agent) => {

          // vision model
          val input = AgentInput(Direction.All.flatMap { d ⇒
            entities.get(pos + d) match {
              case None ⇒ Some(ViewEmpty(1f, d))
              case Some(Food) ⇒ Some(ViewFood(1f, d))
              case Some(Block) ⇒ None
              case Some(a: Agent) ⇒ Some(ViewAgent(agent.relatedness(a), d))
            }
          })

          val output = agent.decide(input)

          val (newPos, newAgent) = output match {
            case AgentOutput(signals) ⇒
              signals.maxBy(_.å) match {
                case Rest(_) ⇒ (pos, agent.vitalize(-.01f))
                case Move(_, d) ⇒ entities.get(pos + d) match {
                  case Some(Food) ⇒
                    (pos + d, agent.vitalize(.1f)) // move to food
                  case None ⇒
                    (pos + d, agent.vitalize(-.1f)) // move
                }
                case Attack(_, d) ⇒ entities.get(pos + d) match {
                  case Some(_: Agent) ⇒
                    entities.remove(pos + d)
                    (pos + d, agent.vitalize(.1f))
                }
                case Divide(_, d) ⇒ entities.get(pos + d) match {
                  case None ⇒
                    val (a0, a1) = agent.divide
                    entities.update(pos + d, a0) 
                    (pos, a1)
                }
              }
          }

          entities.remove(pos)
          entities.update(newPos, newAgent)
        }
        case _ => ()
      }
    }

    val newFoodPos = HexPosition.random
    if(!entities.contains(newFoodPos)) {
      entities.update(newFoodPos, Food)
    }
  }
}


class Life extends Game {
  val HEX_RADIUS = 9.0f
  val AGENT_RADIUS = 8.0f
  val FOOD_RADIUS = 3.0f
  val BLOCK_RADIUS = 5.0f

  lazy val camera = new OrthographicCamera
  lazy val shapeRenderer = new ShapeRenderer

  var state = new WorldState(
    mutable.HashMap(
      (1 to 100).map(_ => HexPosition.random -> Agent()) ++
      (1 to 400).map(_ => HexPosition.random -> Food) ++
      (0 until 95).flatMap(x ⇒ Seq(HexPosition(x, 0) → Block, HexPosition(x, 46) → Block)) ++
      (0 until 47).flatMap(y ⇒ Seq(HexPosition(0, y) → Block, HexPosition(94, y) → Block))
    :_*)
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

    for ((p,e) <- state.entities) {
      e match {
        case agent: Agent => {
          val v = p.vector * HEX_RADIUS
          shapeRenderer.begin(ShapeType.Filled)
          shapeRenderer.setColor(1.0f, 0.592f, 0.243f, 1.0f)
          shapeRenderer.circle(v.x, v.y, AGENT_RADIUS * agent.vitality + 4)
          shapeRenderer.end()
        }
        case Food => {
          val v = p.vector * HEX_RADIUS
          shapeRenderer.begin(ShapeType.Filled)
          shapeRenderer.setColor(0.310f, 0.788f, 0.812f, 1)
          shapeRenderer.circle(v.x, v.y, FOOD_RADIUS)
          shapeRenderer.end()
        }
        case Block ⇒ {
//          val v = p.vector * HEX_RADIUS
//          shapeRenderer.begin(ShapeType.Filled)
//          shapeRenderer.setColor(1.00f, 0.110f, 0.086f, 1)
//          shapeRenderer.circle(v.x, v.y, BLOCK_RADIUS)
//          shapeRenderer.end()
        }
      }
    }
  }
}
