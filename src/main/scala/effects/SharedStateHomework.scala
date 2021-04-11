package effects

import cats.Monad
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

import scala.concurrent.duration
import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 */
object SharedStateHomework extends IOApp {

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]
    def put(key: K, value: V): F[Unit]
  }

  type CacheData[K, V] = Map[K, (Long, V)]
  type CacheEntry[K, V] = (K, (Long, V))
  type CacheState[F[_], K, V] = Ref[F, Map[K, (Long, V)]]

  class RefCache[F[_] : Clock : Monad, K, V](state: CacheState[F, K, V], expiresIn: FiniteDuration) extends Cache[F, K, V] {
    def get(key: K): F[Option[V]] = {
      val ifNotExpired = (time: Long) => state.modify { map =>
        map.get(key) match {
          case Some(entry) if expired(time, (key, entry), expiresIn) => (map.removed(key), None)
          case Some((_, value)) => (map, Some(value))
          case None => (map, None)
        }
      }

      time >>= ifNotExpired
    }

    def put(key: K, value: V): F[Unit] = {
      val put = (entry: CacheEntry[K, V]) => state.update(_ + entry)
      val entry = (key: K, value: V) => (time: Long) => (key -> (time, value)).pure[F]

      time >>= entry(key, value) >>= put
    }
  }

  object Cache {
    def of[F[_] : Clock, K, V](expiresIn: FiniteDuration, checkOnExpirationsEvery: FiniteDuration)
                              (implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {
      for {
        state <- Ref.of(Map.empty[K, (Long, V)])
        _ <- C.start(expiring(state, expiresIn, checkOnExpirationsEvery))
        ref = new RefCache[F, K, V](state, expiresIn)
      } yield ref
    }

    def expiring[F[_] : Clock, K, V](state: CacheState[F, K, V], expiresIn: FiniteDuration, checkOnExpirationsEvery: FiniteDuration)
                                    (implicit T: Timer[F], C: Concurrent[F]): F[Unit] = {
      val task = for {
        _ <- T.sleep(checkOnExpirationsEvery)
        time <- time
        _ <- state.update(_.filterNot(expired(time, _, expiresIn)))
      } yield ()

      task.foreverM
    }
  }

  private def expired[F[_], K, V](time: Long, entry: CacheEntry[K, V], expiresIn: FiniteDuration): Boolean = {
    val (_, (created, _)) = entry
    (time - created).millis > expiresIn
  }

  private def time[F[_] : Clock]: F[Long] = Clock[F].realTime(duration.MILLISECONDS)

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      cache <- Cache.of[IO, Int, String](10.seconds, 4.seconds)
      _ <- cache.put(1, "Hello")
      _ <- cache.put(2, "World")
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
    } yield ExitCode.Success
  }
}

