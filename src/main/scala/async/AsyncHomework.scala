package async

import cats.implicits.toTraverseOps

import java.net.URL
import java.util.concurrent.Executors
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source

/**
 * Application:
 * - takes a web-page URL from arguments (args array)
 * - loads the web-page body, extracts HTTP links from it
 * - for all the found links, tries to fetch a server name header if there is one
 * - prints all the encountered unique server name values in alphabetical order
 *
 * Each link processing should be done in parallel.
 * Validation of arguments is not needed.
 *
 * Try to test it on http://google.com!
 */
object AsyncHomework extends App {
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  val url = "http://google.com" //args.head
  val res = for {
    body <- fetchPageBody(url)
    links <- findLinkUrls(body)
    serverNames <- links.traverse(fetchServerName)
  } yield serverNames

  Await.result(res, 5.seconds).sorted.distinct.foreach {
    case Some(value) => println(value)
    case None => ()
  }

  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)
      try {
        source.mkString
      } finally {
        source.close()
      }
    }
  }

  private def fetchServerName(url: String): Future[Option[String]] = {
    println(s"Fetching server name header for $url")
    Future {
      //println(Thread.currentThread().toString)
      Option(new URL(url).openConnection().getHeaderField("Server"))
    }
  }

  private def findLinkUrls(html: String): Future[List[String]] = Future {
    val linkPattern = """href="(http[^"]+)"""".r
    linkPattern.findAllMatchIn(html).map(m => m.group(1)).toList
  }
}
