package life

import scala.collection.mutable.ArrayBuffer
import scala.math.{sin, cos, toRadians, sqrt}
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
  pos: HexPosition,
  vitality: Float
) extends Entity {
  def decide(input: AgentInput): Action = {
    Action.Move(input.food.headOption.getOrElse(Direction.RightDown))
  }
}

object Agent {
  def random: Agent = Agent(HexPosition(Random.nextInt(128), Random.nextInt(72)), 1.0f - Random.nextFloat * 0.75f)
}

case class Food(
  pos: HexPosition
)

object Food {
  def random: Food = Food(HexPosition(Random.nextInt(128), Random.nextInt(72)))
}

case class AgentInput(
  food: Set[Direction]
)

class WorldState(
  var agents: ArrayBuffer[Agent],
  var foods: ArrayBuffer[Food]
) {
  def step(): Unit = {
    /*val action = (
      Gdx.input.isKeyJustPressed(Keys.E),
      Gdx.input.isKeyJustPressed(Keys.W),
      Gdx.input.isKeyJustPressed(Keys.Q),
      Gdx.input.isKeyJustPressed(Keys.A),
      Gdx.input.isKeyJustPressed(Keys.S),
      Gdx.input.isKeyJustPressed(Keys.D)
    ) match {
      case (true, false, false, false, false, false) => Action.Move(Direction.RightUp)
      case (false, true, false, false, false, false) => Action.Move(Direction.Up)
      case (false, false, true, false, false, false) => Action.Move(Direction.LeftUp)
      case (false, false, false, true, false, false) => Action.Move(Direction.LeftDown)
      case (false, false, false, false, true, false) => Action.Move(Direction.Down)
      case (false, false, false, false, false, true) => Action.Move(Direction.RightDown)
      case _ => Action.Wait
    }*/

    for(i <- 0 until agents.length) {
      if(i < agents.length) {
        val agent = agents(i)
        val input = AgentInput(Direction.All.filter(d => foods.exists(f => f.pos == agent.pos + d)).toSet)
        val action = agent.decide(input)
        val newAgent = action match {
          case Action.Move(d) => {
            if(!agents.exists(a => a.pos == agent.pos + d)) {
              if(food.exists(a
              agent.copy(pos = agent.pos + d)
            } else {
              agent
            }
          }
          case _ => agent
        }
        agents.update(i, newAgent)
      }
    }

    val action: Action = Action.Wait

    foods ++= { if(Random.nextFloat > 0.95f) { Some(Food.random) } else { None } }
  }
}


class Life extends Game {
  val HEX_RADIUS = 15.0f
  val AGENT_RADIUS = 7.0f
  val FOOD_RADIUS = 4.0f

  lazy val camera = new OrthographicCamera;
  lazy val shapeRenderer = new ShapeRenderer;

  var state = new WorldState(
    agents = ArrayBuffer((1 to 100).map(_ => Agent.random):_*),
    foods = ArrayBuffer((1 to 100).map(_ => Food.random):_*)
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
        case 0 => (0.3f, 0.0f, 0.0f)
        case 1 => (0.0f, 0.3f, 0.0f)
        case 2 => (0.3f, 0.3f, 0.0f)
        case _ => (0.0f, 0.0f, 0.3f)
      }
      shapeRenderer.setColor(r, g, b, 1.0f)
      val v = HexPosition(x, y).vector * HEX_RADIUS
      for((x0,y0,x1,y1,x2,y2) <- hexagonTriangles) {
        shapeRenderer.triangle(x0 + v.x, y0 + v.y, x1 + v.x, y1 + v.y, x2 + v.x, y2 + v.y)
      }
    }
    shapeRenderer.end()

    for(agent <- state.agents) {
      val v = agent.pos.vector * HEX_RADIUS
      shapeRenderer.begin(ShapeType.Filled)
      shapeRenderer.setColor(agent.vitality * 0.2f + 0.6f, agent.vitality * 0.8f, agent.vitality * 0.6f, 1)
      shapeRenderer.circle(v.x, v.y, AGENT_RADIUS)
      shapeRenderer.end()
    }

    for(food <- state.foods) {
      val v = food.pos.vector * HEX_RADIUS
      shapeRenderer.begin(ShapeType.Filled)
      shapeRenderer.setColor(0.7f, 0.9f, 0.6f, 1)
      shapeRenderer.circle(v.x, v.y, FOOD_RADIUS)
      shapeRenderer.end()
    }
  }
}
