package com.training.async

import java.net.URL
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.{Failure, Success}

object AsyncHomeworkApp extends App {
  private val url = args.headOption.getOrElse("https://google.com")

  private implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  private val impl = new Impl

  impl.fetchUniqueSortedServerNamesFromPageLinks(url).onComplete {
    case Success(names) =>
      names.foreach(println)
      sys.exit(0)
    case Failure(t)     =>
      t.printStackTrace()
      sys.exit(1)
  }

  private class Impl extends AsyncHomework {
    override protected implicit def ec: ExecutionContext = AsyncHomeworkApp.ec

    override protected def fetchPageBody(url: String): Future[String] = {
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

    override protected def fetchServerName(url: String): Future[Option[String]] = {
      println(s"Fetching server name header for $url")
      Future {
        Option(new URL(url).openConnection().getHeaderField("Server"))
      }
    }

    override protected def findLinkUrls(html: String): Future[Vector[String]] = Future {
      val linkPattern = """href="(http[^"]+)"""".r
      linkPattern.findAllMatchIn(html).map(m => m.group(1)).toVector
    }
  }
}
