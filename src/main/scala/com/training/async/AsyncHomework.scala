package com.training.async

import scala.concurrent.{ExecutionContext, Future}

/**
 * Implement logic:
 * - take a web-page URL
 * - load the web-page body, extracts HTTP links from it
 * - for all the found links, try to fetch a server name header if there is one
 * - return all the encountered unique server name values in alphabetical order
 *
 * Each link processing should be done in parallel.
 * Validation of arguments is not needed.
 *
 * Try to test it on https://google.com using AsyncHomeworkApp!
 */
abstract class AsyncHomework {
  protected implicit def ec: ExecutionContext

  //-- these methods are provided to you!
  protected def fetchPageBody(url: String): Future[String]
  protected def fetchServerName(url: String): Future[Option[String]]
  protected def findLinkUrls(html: String): Future[Vector[String]]
  //--

  final def fetchUniqueSortedServerNamesFromPageLinks(url: String): Future[Vector[String]] = {
    val futureLinks = for {
      html <- fetchPageBody(url)
      links <- findLinkUrls(html)
    } yield links

    futureLinks
      .map(links => Future.traverse(links)(link => fetchServerName(link)))
      .flatten.map(optionNames => {
        optionNames.flatten.distinct.sorted
      })
  }
}
