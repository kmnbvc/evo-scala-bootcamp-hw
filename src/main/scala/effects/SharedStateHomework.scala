package effects

import cats.Monad
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

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

  type CacheState[F[_], K, V] = Ref[F, Map[K, (Long, V)]]

  class RefCache[F[_] : Clock : Monad, K, V](state: Ref[F, Map[K, (Long, V)]], expiresIn: FiniteDuration) extends Cache[F, K, V] {
    def get(key: K): F[Option[V]] = {
      state.modify { values =>
        val value = values.get(key)
        val expired = value.exists {
          case (created, _) =>
            val lifetime = System.currentTimeMillis() - created
            lifetime.millis > expiresIn
        }
        val newState = if (expired) values.removed(key) else values
        val result = value.filterNot(_ => expired).map(_._2)
        (newState, result)
      }
    }

    def put(key: K, value: V): F[Unit] = {
      for {
        time <- time()
        state <- state.getAndUpdate(_ + (key -> (time, value)))
      } yield state.void
    }

    private def time(): F[Long] = Clock[F].realTime(expiresIn.unit)
  }

  object Cache {
    def of[F[_] : Clock, K, V](expiresIn: FiniteDuration, checkOnExpirationsEvery: FiniteDuration)
                              (implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {

      for {
        state <- Ref.of(Map.empty[K, (Long, V)])
        exp <- expiring(state, expiresIn, checkOnExpirationsEvery)
        ref = new RefCache[F, K, V](exp, expiresIn)
      } yield ref
    }

    def expiring[F[_] : Clock, K, V](state: CacheState[F, K, V], expiresIn: FiniteDuration, checkOnExpirationsEvery: FiniteDuration)
                     (implicit T: Timer[F], C: Concurrent[F]): F[CacheState[F, K, V]] = {
      val clean = (time: Long) => state.getAndUpdate { values =>
        values.filter {
          case (_, (created, _)) => (time - created).millis < expiresIn
        }
      }.as(state)

      C.defer(Clock[F].realTime(expiresIn.unit))
        .flatMap(clean)
        .productL(T.sleep(checkOnExpirationsEvery))
        .foreverM
    }
  }

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

