package typeclass

import typeclass.ImplicitsHomework.SuperVipCollections4s._
import typeclass.ImplicitsHomework.SuperVipCollections4s.instances._
import typeclass.ImplicitsHomework.SuperVipCollections4s.syntax._

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object ImplicitsHomework {

  /**
   * Lo and behold! Brand new super-useful collection library for Scala!
   *
   * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
   * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
   * of the data stored.
   *
   * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
   * a thing called size score. Its calculation rules:
   * - size score of a Byte is 1
   * - Int - 4 (as primitive JVM int consists of 4 bytes)
   * - Long - 8
   * - Char - 2 (one UTF-16 symbol is 2 bytes)
   * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
   * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
   * the fields
   * - score for any sequence (Array[T], List[T], Vector[T]) is
   * 12 (our old friend object header) + sum of scores of all elements
   * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
   */
  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object GetSizeScore {
      def apply[T: GetSizeScore]: GetSizeScore[T] = implicitly
    }

    object syntax {
      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = GetSizeScore[T].apply(inner)
      }
    }

    /**
     * Mutable key-value cache which limits the size score of the data scored.
     *
     * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
     * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
     * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
     * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
     *
     * @param maxSizeScore max size score for the stored data
     * @tparam K key type
     * @tparam V value type
     */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
      private val map = mutable.LinkedHashMap.empty[K, V]

      def get(key: K): Option[V] = map.get(key)

      def put(key: K, value: V): Unit = {
        if (score(key -> value) <= maxSizeScore) {
          val evc = evict(map.toList, score(key -> value), Nil)
          map.subtractAll(evc)
          map.put(key, value)
        }
      }

      @tailrec
      private def evict(cache: List[(K, V)], require: SizeScore, result: List[K]): List[K] = cache match {
        case Nil => result
        case (key, _) :: tail =>
          if (free(cache) >= require) result
          else evict(tail, require, key :: result)
      }

      private def free(cache: List[(K, V)]): SizeScore = maxSizeScore - score(cache)
      private def score(l: List[(K, V)]): SizeScore = l.map(score).sum
      private def score(entry: (K, V)): SizeScore = {
        val (k, v) = entry
        k.sizeScore + v.sizeScore
      }
    }

    /**
     * Cool custom immutable multi-map collection - does not extend the standard library collection types
     * (yes, this is a feature)
     */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()
      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /**
     * Type-class allowing us to iterate over different "collection-like" types with one type arg
     */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    /**
     * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
     */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]
      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object instances {

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }

      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }

      implicit val mapIterate2: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keysIterator
        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.valuesIterator
      }

      implicit val packedMultiMapIterate2: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.map(_._1).iterator
        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.map(_._2).iterator
      }

      implicit val sizeScoreByte: GetSizeScore[Byte] = _ => 1
      implicit val sizeScoreInt: GetSizeScore[Int] = _ => 4
      implicit val sizeScoreLong: GetSizeScore[Long] = _ => 8
      implicit val sizeScoreChar: GetSizeScore[Char] = _ => 2

      implicit val sizeScoreString: GetSizeScore[String] = s => 12 + s.length * 2

      implicit def sizeScoreIterate[F[_] : Iterate, T : GetSizeScore]: GetSizeScore[F[T]] = it => {
        val ev = implicitly[Iterate[F]]
        ev.iterator(it).map(_.sizeScore).sum + 12
      }

      implicit def sizeScoreIterate2[F[_, _] : Iterate2, K: GetSizeScore, V: GetSizeScore]: GetSizeScore[F[K, V]] = cc => {
        val ev = implicitly[Iterate2[F]]
        ev.iterator1(cc).map(_.sizeScore).sum + ev.iterator2(cc).map(_.sizeScore).sum + 12
      }
    }

    // can be skipped from review since this is probably topic of some future lecture
    // and at this point I barely understand this mystery
    object VeryGenericSomething {
      import shapeless._
      import shapeless.ops.hlist._

      def toHList[P <: Product, L <: HList](p: P)(implicit gen: Generic.Aux[P, L]): L = gen.to(p)

      def score[A <: HList, B <: HList](a: A)(implicit ev1: LiftAll.Aux[GetSizeScore, A, B],
                                              ev2: ToTraversable.Aux[B, List, GetSizeScore[_]]): SizeScore = {
        a.runtimeList.zip(ev1.instances.toList).map {
          case (field, g) => g.asInstanceOf[GetSizeScore[Any]].apply(field)
        }.sum
      }
    }
  }

  /*
   * Time to bring some business value!
   * #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {

    final case class Tweet(id: Long,
                           userId: Int,
                           hashTags: Vector[String],
                           attributes: PackedMultiMap[String, String],
                           fbiNotes: List[FbiNote])

    final case class FbiNote(month: String,
                             favoriteChar: Char,
                             watchedPewDiePieTimes: Long)

    trait TweetCache {
      def put(tweet: Tweet): Unit
      def get(id: Long): Option[Tweet]
    }

    class MutableBoundedTweetCache(maxSizeScore: Int) extends TweetCache {
      import VeryGenericSomething._

      implicit val sizeScoreTweet: GetSizeScore[Tweet] = tweet => score(toHList(tweet))
      implicit val sizeScoreFbiNote: GetSizeScore[FbiNote] = note => score(toHList(note))

      private val cache: MutableBoundedCache[Long, Tweet] = new MutableBoundedCache[Long, Tweet](maxSizeScore)

      override def put(tweet: Tweet): Unit = cache.put(tweet.id, tweet)
      override def get(id: Long): Option[Tweet] = cache.get(id)
    }

    /*
     * Return an implementation based on MutableBoundedCache[Long, Tweet]
     */
    def createTweetCache(maxSizeScore: SizeScore): TweetCache = new MutableBoundedTweetCache(maxSizeScore)
  }

}

// Additional exercises from Q&A session
object QnAExercises {

  import cats._

  // 4.4. Implement Semigroupal for Map
  implicit def semigroupalMap[K]: Semigroupal[Map[K, *]] = new Semigroupal[Map[K, *]] {
    override def product[A, B](fa: Map[K, A], fb: Map[K, B]): Map[K, (A, B)] = (fa, fb) match {
      case (x, y) => for { (k, v1) <- x if y.contains(k) } yield (k, (v1, y(k)))
      case _ => Map.empty
    }
  }

  // 5. Applicative
  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](x: A): F[A]
  }

  // 5.1. Implement Applicative for Option, Either
  implicit val applicativeEither: Applicative[Either[String, *]] = new Applicative[Either[String, *]] {
    override def pure[A](x: A): Either[String, A] = Right(x)
    override def product[A, B](fa: Either[String, A], fb: Either[String, B]): Either[String, (A, B)] = (fa, fb) match {
      case (Right(a), Right(b)) => Right(a, b)
      case (Left(s1), Left(s2)) => Left(s1 + s2)
      case (Left(s1), _) => Left(s1)
      case (_, Left(s2)) => Left(s2)
    }
    override def map[A, B](fa: Either[String, A])(f: A => B): Either[String, B] = fa match {
      case Right(a) => Right(f(a))
      case _ => fa.asInstanceOf
    }
  }

  implicit val applicativeOption: Applicative[Option] = new Applicative[Option] {
    override def pure[A](x: A): Option[A] = Some(x)
    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = Semigroupal[Option].product(fa, fb)
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = Functor[Option].fmap(fa)(f)
  }

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  def traverse[F[_] : Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
    val ev = implicitly[Applicative[F]]
    val append = (x: (List[B], B)) => x._2 :: x._1
    as.map(f).foldLeft(ev.pure(List[B]()))((acc, fb) => ev.fmap(ev.product(acc, fb))(append))
  }

  // 6. Foldable
  // 6.1. Implement Foldable with `foldLeft`
  trait Foldable[F[_]] {
    def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B
  }

  // 6.2. Implement Foldable for List
  // Note: we can use here foldLeft from standard library
  implicit val foldableList: Foldable[List] = new Foldable[List] {
    override def foldLeft[A, B](fa: List[A], z: B)(f: (B, A) => B): B = fa.foldLeft(z)(f)
  }

  // 6.3. Implement `traverse` for all Foldables instead of List

  // is it supposed to be implemented without MonoidK?
  def traverse2[F[_], L[_], A, B](as: L[A])(f: A => F[B])(
    implicit ev: Applicative[F], fld: Foldable[L], apl: Applicative[L], mon: MonoidK[L]): F[L[B]] = {

    fld.foldLeft(apl.fmap(as)(f), ev.pure(mon.empty[B])) { (acc, fb) =>
      ev.fmap(ev.product(acc, fb)) {
        case (acc, b) => mon.combineK(acc, apl.pure(b))
      }
    }
  }
}
