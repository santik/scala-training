package com.training.async

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.util.UUID
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.Random

class AsyncHomeworkTest extends AnyFreeSpec with Matchers {
  private implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  private val futureResultTimeout = 20.seconds

  "fetchUniqueSortedServerNamesFromPageLinks" in {
    val impl = new TestImpl
    val futureResult = impl.fetchUniqueSortedServerNamesFromPageLinks(impl.topLevelUrl)
    try {
      val result = Await.result(futureResult, futureResultTimeout)
      result shouldEqual impl.serverNames
    } catch {
      case _: TimeoutException =>
        fail(
          s"result can't be obtained in $futureResultTimeout - dead-lock or not doing things in parallel?",
        )
    }
  }

  private class TestImpl extends AsyncHomework {
    val runId: String = UUID.randomUUID().toString
    val topLevelUrl: String = s"https://test-$runId"
    val testBody: String = s"test-body-$runId"
    //10 unique server names, sorted
    val serverNames: Vector[String] = Vector(
      "server-Aronne",
      "server-Beli",
      "server-Ewan",
      "server-Foteini",
      "server-Goemon",
      "server-Ksenija",
      "server-Lihi",
      "server-Nicanor",
      "server-Uschi",
      "server-Walter",
    )
    private val linkUrlToServerName: Map[String, String] = {
      val links = 1.to(serverNames.size * 10).map(i => f"link-$runId-$i%03d").toVector
      val serverNamesShuffled = Random.shuffle(serverNames)
      links.zipWithIndex.map {
        case (link, i) => link -> serverNamesShuffled(i % serverNamesShuffled.size)
      }.toMap
    }
    private val links: Vector[String] = Random.shuffle(linkUrlToServerName.keys.toVector)

    override protected implicit def ec: ExecutionContext = AsyncHomeworkTest.this.ec
    override protected def fetchPageBody(url: String): Future[String] = {
      url shouldEqual topLevelUrl
      Future {
        sleep1s()
        testBody
      }
    }
    override protected def fetchServerName(url: String): Future[Option[String]] = Future {
      sleep1s()
      linkUrlToServerName.get(url)
    }
    override protected def findLinkUrls(html: String): Future[Vector[String]] = {
      html shouldEqual testBody
      Future {
        sleep1s()
        links
      }
    }
  }

  private def sleep1s(): Unit = {
    blocking {
      Thread.sleep(1000L)
    }
  }
}
