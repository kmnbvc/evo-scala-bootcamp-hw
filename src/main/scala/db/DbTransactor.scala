package db


import cats.effect.{Async, Blocker, ContextShift, Resource}
import doobie.Transactor
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts

object DbTransactor {

  val dbDriverName = "org.h2.Driver"
  val dbUrl = "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1"
  val dbUser = ""
  val dbPwd = ""

  def pooled[F[_] : ContextShift : Async]: Resource[F, Transactor[F]] =
    for {
      ce <- ExecutionContexts.fixedThreadPool[F](10)
      be <- Blocker[F]
      xa <- HikariTransactor.newHikariTransactor[F](
        driverClassName = dbDriverName,
        url = dbUrl,
        user = dbUser,
        pass = dbPwd,
        ce,
        be,
      )
    } yield xa
}
