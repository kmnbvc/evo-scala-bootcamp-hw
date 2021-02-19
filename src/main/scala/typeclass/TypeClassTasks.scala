package typeclass

object TypeClassTask {
  type HashCode[T] = Function[T, Int]

  implicit class HashCodeSyntax[A : HashCode](x: A)(implicit magic: HashCode[A]) {
    def hash: Int = magic.apply(x)
  }

  implicit val hashCodeString: HashCode[String] = s => s.hashCode
  "abc".hash
}

object Task1 {
  final case class Money(amount: BigDecimal)

  implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)
}

object Task2 {
  trait Show[T] {
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  implicit val showUser: Show[User] = _.name

  implicit class ShowSyntax[T : Show](entity: T)(implicit ev: Show[T]) {
    def show: String = ev.show(entity)
  }

  User("1", "Oleg").show
}

object Task3 {
  type Error = String
  trait Parse[T] {
    def parse(entity: String): Either[Error, T]
  }

  final case class User(id: String, name: String)

  private val parseUserFields = (s: String) => Either.cond(s.startsWith("User(") && s.endsWith(")"),
    s.substring(5, s.length - 1).split(','),
    s"cannot parse $s to a user: unknown format")

  implicit val parseUser: Parse[User] = s => parseUserFields(s).flatMap {
    case Array(id, name) if id.nonEmpty && name.nonEmpty => Right(User(id, name))
    case _ => Left(s"string $s is not a valid user")
  }

  implicit class ParseSyntax(entity: String) {
    def parse[T : Parse](implicit ev: Parse[T]): Either[Error, T] = ev.parse(entity)
  }

  "lalala".parse[User]
}

object Task4 {
  implicit class EqSyntax[T](entity: T) {
    // getting error when invoke '===' from test
    def ====(other: T): Boolean = entity == other
  }

  1 ==== 2
  "1" ==== "2"
//  1 ==== "3"
}

object AdvancedHomework {
  trait FlatMap[F[_]] {
    def _flatMap[A, B](entity: F[A])(f: A => F[B]): F[B]
  }

  implicit class FlatMapSyntax[F[_] : FlatMap, A](entity: F[A])(implicit ev: FlatMap[F]) {
    def _flatMap[B](f: A => F[B]): F[B] = ev._flatMap(entity)(f)
  }

  case class User(id: String, name: String)
  case class Admin(id: String, name: String, permission: String)

  implicit val optionFlatMap: FlatMap[Option] = new FlatMap[Option] {
    override def _flatMap[A, B](entity: Option[A])(f: A => Option[B]): Option[B] = entity.flatMap(f)
  }

  Option(User("1", "aaa"))._flatMap {
    case User("1", name) => Some(Admin("1", name, "hello"))
    case _ => None
  }
}
