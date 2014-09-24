package life.math

import scala.math.{Pi,sin,cos,sqrt}

object MathConstants {
  val Tau = (Pi * 2.0).toFloat
}

object Matrix3x3 {
  val identity = Matrix3x3(
    Vector3(1, 0, 0),
    Vector3(0, 1, 0),
    Vector3(0, 0, 1)
  )

  def translate(x: Float, y: Float) = Matrix3x3(
    Vector3(1, 0, 0),
    Vector3(0, 1, 0),
    Vector3(x, y, 1)
  )

  def scale(x: Float, y: Float) = Matrix3x3(
    Vector3(x, 0, 0),
    Vector3(0, y, 0),
    Vector3(0, 0, 1)
  )

  def rotate(a: Float) = {
    val c = cos(a).toFloat
    val s = sin(a).toFloat
    Matrix3x3(
      Vector3( c, s, 0),
      Vector3(-s, c, 0),
      Vector3( 0, 0, 1)
    )
  }
}

case class Matrix3x3(v0: Vector3, v1: Vector3, v2: Vector3) {
  def *(o: Matrix3x3) = {
    Matrix3x3(
      Vector3(
        v0.x * o.v0.x + v1.x * o.v0.y + v2.x * o.v0.z,
        v0.y * o.v0.x + v1.y * o.v0.y + v2.y * o.v0.z,
        v0.z * o.v0.x + v1.z * o.v0.y + v2.z * o.v0.z
      ),
      Vector3(
        v0.x * o.v1.x + v1.x * o.v1.y + v2.x * o.v1.z,
        v0.y * o.v1.x + v1.y * o.v1.y + v2.y * o.v1.z,
        v0.z * o.v1.x + v1.z * o.v1.y + v2.z * o.v1.z
      ),
      Vector3(
        v0.x * o.v2.x + v1.x * o.v2.y + v2.x * o.v2.z,
        v0.y * o.v2.x + v1.y * o.v2.y + v2.y * o.v2.z,
        v0.z * o.v2.x + v1.z * o.v2.y + v2.z * o.v2.z
      )
    )
  }

  def *(v: Vector2): Vector2 = {
    val p = this * Vector3(v.x, v.y, 1)
    Vector2(p.x / p.z, p.y / p.z)
  }

  def *(v: Vector3): Vector3 = {
    Vector3(
      v0.x * v.x + v1.x * v.y + v2.x * v.z,
      v0.y * v.x + v1.y * v.y + v2.y * v.z,
      v0.z * v.x + v1.z * v.y + v2.z * v.z
    )
  }
}

object Vector2 {
  val zero = Vector2(0, 0)
}

case class Vector2(x: Float, y: Float) {
  def *(s: Float) = Vector2(s * x, s * y)
  def +(o: Vector2) = Vector2(x + o.x, y + o.y)
  def -(o: Vector2) = Vector2(x - o.x, y - o.y)
  def mag: Float = sqrt(x*x + y*y).toFloat
}

object Vector3 {
  val zero = Vector3(0, 0, 0)
}

case class Vector3(x: Float, y: Float, z: Float) {
  def *(s: Float) = Vector3(s * x, s * y, s * z)
  def +(o: Vector3) = Vector3(x + o.x, y + o.y, z + o.z)
  def -(o: Vector3) = Vector3(x - o.x, y - o.y, z - o.z)
}

object Implicits {
  implicit class VectorOps(f: Float) {
    def *(v: Vector2) = Vector2(f * v.x, f * v.y)
    def *(v: Vector3) = Vector3(f * v.x, f * v.y, f * v.z)
  }
}
