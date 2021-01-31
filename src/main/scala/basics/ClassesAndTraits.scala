package basics

object ClassesAndTraits {

  sealed trait Shape extends Located with Bounded {
    def area: Double
  }

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Movable[+S] {
    this: S =>
    def move(dx: Double, dy: Double): S
  }

  final case class Point(x: Double, y: Double) extends Shape with Movable[Point] {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y

    override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)
    override def area: Double = 0
  }

  sealed abstract class RectangleBase(center: Point, width: Double, height: Double) extends Shape {
    override def x: Double = center.x
    override def y: Double = center.y
    override def minX: Double = center.x - width / 2
    override def maxX: Double = center.x + width / 2
    override def minY: Double = center.y - height / 2
    override def maxY: Double = center.y + height / 2

    override def area: Double = (maxX - minX) * (maxY - minY)
  }

  sealed trait RectangleMovable[+S] extends RectangleBase with Movable[S] {
    this: S =>
    def at(p: Point): S
    override def move(dx: Double, dy: Double): S = at(Point(this.x + dx, this.y + dy))
  }

  final case class Rectangle(center: Point, width: Double, height: Double)
    extends RectangleBase(center, width, height)
      with RectangleMovable[Rectangle] {
    override def at(p: Point): Rectangle = Rectangle(p, width, height)
  }

  final case class Square(center: Point, side: Double)
    extends RectangleBase(center, side, side)
      with RectangleMovable[Square] {
    override def at(p: Point): Square = Square(p, side)
  }

  final case class Triangle(p1: Point, p2: Point, p3: Point) extends Shape with Movable[Triangle] {
    override def x: Double = xs.sum / 3
    override def y: Double = ys.sum / 3
    override def minX: Double = xs.min
    override def maxX: Double = xs.max
    override def minY: Double = ys.min
    override def maxY: Double = ys.max
    // note: implemented that way just for fun
    override def move(dx: Double, dy: Double): Triangle = Triangle.tupled(
      List(p1, p2, p3).map(_.move(dx, dy)) match {
        case p1 :: p2 :: p3 :: Nil => (p1, p2, p3)
        case _ => throw new RuntimeException
      })

    override def area: Double = {
      val (a, b, c) = sidesLen
      val s = (a + b + c) / 2
      Math.sqrt(s * (s - a) * (s - b) * (s - c))
    }

    private def sidesLen: (Double, Double, Double) = (distance(p1, p2), distance(p1, p3), distance(p2, p3))
    private def distance(p1: Point, p2: Point): Double = Math.sqrt(Math.pow(p2.x - p1.x, 2) + Math.pow(p2.y - p1.y, 2))

    private def xs = List(p1, p2, p3).map(_.x)
    private def ys = List(p1, p2, p3).map(_.y)
  }

  sealed trait Shape3D {
    def surfaceArea: Double
    def volume: Double
  }

  sealed trait Movable3D[+S] {
    this: S =>
    def move(dx: Double, dy: Double, dz: Double): S
  }

  case class Origin3D() extends Shape3D {
    override def surfaceArea: Double = 0
    override def volume: Double = 0
  }

  case class Point3D(x: Double, y: Double, z: Double) extends Shape3D with Movable3D[Point3D] {
    override def surfaceArea: Double = 0
    override def volume: Double = 0
    override def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(x + dx, y + dy, z + dz)
  }

  case class Sphere(center: Point3D, radius: Double) extends Shape3D with Movable3D[Sphere] {
    override def surfaceArea: Double = ???
    override def volume: Double = ???
    override def move(dx: Double, dy: Double, dz: Double): Sphere = Sphere(center.move(dx, dy, dz), radius)
  }

  case class Cube(center: Point3D, side: Double) extends Shape3D with Movable3D[Cube] {
    override def surfaceArea: Double = ???
    override def volume: Double = ???
    override def move(dx: Double, dy: Double, dz: Double): Cube = Cube(center.move(dx, dy, dz), side)
  }

  case class Cuboid(center: Point3D, width: Double, height: Double) extends Shape3D with Movable3D[Cuboid] {
    override def surfaceArea: Double = ???
    override def volume: Double = ???
    override def move(dx: Double, dy: Double, dz: Double): Cuboid = Cuboid(center.move(dx, dy, dz), width, height)
  }

  case class Triangle3D(p1: Point3D, p2: Point3D, p3: Point3D, p4: Point3D) extends Shape3D with Movable3D[Triangle3D] {
    override def surfaceArea: Double = ???
    override def volume: Double = ???
    // much fun again
    override def move(dx: Double, dy: Double, dz: Double): Triangle3D = Triangle3D.tupled(
      List(p1, p2, p3, p4).map(_.move(dx, dy, dz)) match {
        case p1 :: p2 :: p3 :: p4 :: Nil => (p1, p2, p3, p4)
        case _ => throw new RuntimeException
      })
  }

}
