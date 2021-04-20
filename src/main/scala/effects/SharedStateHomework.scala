package effects

import cats.Monad
import cats.effect._
import cats.effect.concurrent.Ref
import cats.effect.implicits._
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

  type CacheEntry[K, V] = (K, (Long, V))
  type CacheState[F[_], K, V] = Ref[F, Map[K, (Long, V)]]

  private class RefCache[F[_] : Clock : Monad, K, V](
                                                      state: CacheState[F, K, V],
                                                      expiresIn: FiniteDuration
                                                    ) extends Cache[F, K, V] {
    def get(key: K): F[Option[V]] = time.flatMap { current =>
      state.modify { map =>
        map.get(key) match {
          case Some(entry) if expired(current, (key, entry), expiresIn) => (map.removed(key), None)
          case Some((_, value)) => (map, Some(value))
          case None => (map, None)
        }
      }
    }

    def put(key: K, value: V): F[Unit] = {
      time.map(key -> (_, value)).flatMap(entry => state.update(_ + entry))
    }
  }

  object Cache {
    def of[F[_], K, V](expiresIn: FiniteDuration, checkOnExpirationsEvery: FiniteDuration)
                      (implicit T: Timer[F], C: Concurrent[F]): Resource[F, Cache[F, K, V]] = {

      expiringState[F, K, V](expiresIn, checkOnExpirationsEvery).map { state =>
        new RefCache[F, K, V](state, expiresIn)
      }
    }

    def expiringState[F[_], K, V](expiresIn: FiniteDuration, checkOnExpirationsEvery: FiniteDuration)
                                 (implicit T: Timer[F], C: Concurrent[F]): Resource[F, CacheState[F, K, V]] = {
      Resource.eval(Ref.of(Map.empty[K, (Long, V)])).flatTap { state =>
        val task = for {
          _ <- T.sleep(checkOnExpirationsEvery)
          current <- time[F]
          _ <- state.update(_.filterNot(expired(current, _, expiresIn)))
        } yield ()

        task.foreverM.background
      }
    }
  }

  private def expired[F[_], K, V](time: Long, entry: CacheEntry[K, V], expiresIn: FiniteDuration): Boolean = {
    val (_, (created, _)) = entry
    (time - created).millis > expiresIn
  }

  private def time[F[_] : Clock]: F[Long] = Clock[F].realTime(duration.MILLISECONDS)

  override def run(args: List[String]): IO[ExitCode] = {
    Cache.of[IO, Int, String](10.seconds, 4.seconds).use { cache =>
      for {
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
}

