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
  def random = HexPosition(Random.nextInt(128), Random.nextInt(72))
}

sealed trait Direction
object Direction {
  case object RightUp extends Direction
  case object Up extends Direction
  case object LeftUp extends Direction
  case object LeftDown extends Direction
  case object Down extends Direction
  case object RightDown extends Direction
  val All: Seq[Direction] = Seq(RightUp, Up, LeftUp, LeftDown, Down, RightDown)
}

sealed trait Action
object Action {
  case object Wait extends Action
  case class Move(direction: Direction) extends Action
}

sealed trait Entity

case class Agent(
  vitality: Float
) extends Entity {
  def decide(input: AgentInput): Action = {
    input.food.headOption.map { d => 
      Action.Move(d)
    }.
    getOrElse {
      Action.Move(Direction.All(Random.nextInt(Direction.All.size)))
    }
  }
  def vitalize(delta: Float) = Agent(
    vitality = min(1f, max(0f, (vitality + delta)))
  )
}

object Agent {
  def random: Agent = Agent(1.0f - Random.nextFloat * 0.75f)
}

case class Food() extends Entity

object Food {
  def random: Food = Food()
}

object Block extends Entity

case class AgentInput(
  food: Set[Direction]
)

class WorldState(
  var entities: mutable.HashMap[HexPosition,Entity]
) {
  def step(): Unit = {

    for(pos <- entities.keys) {
      entities.get(pos) match {
        case Some(agent: Agent) => {
          val input = AgentInput(Direction.All.filter(d => entities.get(pos + d) == Some(Food())).toSet)
          val action = agent.decide(input)
          val (newPos, newAgent) = action match {
            case Action.Move(d) => {
              entities.get(pos + d) match {
                case Some(_: Agent) => {
                  (pos, agent)
                }
                case Some(_: Food) => {
                  (pos + d, agent.vitalize(0.1f))
                }
                case Some(Block) => {
                  (pos, agent)
                }
                case _ => {
                  (pos + d, agent.vitalize(-0.001f))
                }
              }
            }
            case Action.Wait => {
              (pos, agent.vitalize(0.01f))
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
      entities.update(newFoodPos, Food.random)
    }
  }
}


class Life extends Game {
  val HEX_RADIUS = 9.0f
  val AGENT_RADIUS = 4.0f
  val FOOD_RADIUS = 2.0f
  val BLOCK_RADIUS = 5.0f

  lazy val camera = new OrthographicCamera;
  lazy val shapeRenderer = new ShapeRenderer;

  var state = new WorldState(
    mutable.HashMap((
      (1 to 100).map(_ => HexPosition.random -> Agent.random) ++
      (1 to 400).map(_ => HexPosition.random -> Food.random) ++
      (0 until 93).flatMap(x ⇒ Seq((HexPosition(x, 0) → Block), (HexPosition(x, 45) → Block))) ++
      (0 until 45).flatMap(y ⇒ Seq((HexPosition(0, y) → Block), (HexPosition(93, y) → Block)))
    ):_*)
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

    for((p,e) <- state.entities) {
      e match {
        case agent: Agent => {
          val v = p.vector * HEX_RADIUS
          shapeRenderer.begin(ShapeType.Filled)
          shapeRenderer.setColor(1.0f, 0.592f, 0.243f, 1.0f)
          shapeRenderer.circle(v.x, v.y, AGENT_RADIUS * agent.vitality)
          shapeRenderer.end()
        }
        case food: Food => {
          val v = p.vector * HEX_RADIUS
          shapeRenderer.begin(ShapeType.Filled)
          shapeRenderer.setColor(0.310f, 0.788f, 0.812f, 1)
          shapeRenderer.circle(v.x, v.y, FOOD_RADIUS)
          shapeRenderer.end()
        }
        case Block ⇒ {
          val v = p.vector * HEX_RADIUS
          shapeRenderer.begin(ShapeType.Filled)
          shapeRenderer.setColor(1.00f, 0.110f, 0.086f, 1)
          shapeRenderer.circle(v.x, v.y, BLOCK_RADIUS)
          shapeRenderer.end()
        }
      }
    }
  }
}
