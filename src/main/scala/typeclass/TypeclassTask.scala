package typeclass

object TypeclassTask extends App {

  // not sure it still can be considered a typeclass
  type HashCode[T] = Function[T, Int]

  implicit class HashCodeSyntax[A : HashCode](x: A)(implicit magic: HashCode[A]) {
    def hash: Int = magic.apply(x)
  }

  implicit val hashCodeString: HashCode[String] = s => s.hashCode
  println("abc".hash)
}
