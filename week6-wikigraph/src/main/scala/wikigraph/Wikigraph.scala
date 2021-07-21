package wikigraph

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import Articles.ArticleId
import wikigraph.implementations.Title

/** Analyze the graph of Wikipedia Articles
  *
  * @param client
  *   the wikipedia client providing access to the data.
  */
final class Wikigraph(client: Wikipedia):

  /** Retrieves the names of the articles linked in a page.
    *
    * @param of
    *   the id of the page from which the links are retrieved
    *
    * Hint: Use the methods that you implemented in WikiResult.
    */
  def namedLinks(of: ArticleId): WikiResult[Set[String]] =
    client
      .linksFrom(of)
      .flatMap(articleIds => WikiResult.traverse(articleIds.toSeq)(client.nameOfArticle))
      .map(_.toSet)

  /** Computes the distance between two articles using breadth first search.
    *
    * @param start
    *   compute the distance from this node to `target`
    * @param target
    *   compute the distance from `start` to this node
    * @param maxDepth
    *   stop if the depth exceeds this value
    *
    * @return
    *   an asynchronous computation that might fail. If the maximal distance is exceeded during the search, the result
    *   is None
    *
    * Note: if a domain error occurs when jumping from node to node, fallback by ignoring the problematic node. On the
    * other hand, any system failure just ends the algorithm by returning that system failure.
    *
    * Hint: More information is provided in the description of the assignment Use the `enqueue` and `dequeue` methods of
    * `Queue`.
    */
  def breadthFirstSearch(
      start: ArticleId,
      target: ArticleId,
      maxDepth: Int
    ): WikiResult[Option[Int]] =
    import scala.collection.immutable.Queue

    /** This recursive method iterates on the graph.
      *
      * The algorithm is detailed in the assignment description.
      *   - When the queue is empty or the maxDepth is exceeded (in the next element of the queue), the search fails
      *     with None
      *   - Otherwise a node is retrieved from the queue and its neighbors fetched from the dataset. The search succeeds
      *     if `target` is in this set. Otherwise we recursively search after modifying `unknowns` and adding the
      *     unknown neighbors to the queue with the correct distance.
      *
      * @param visited
      *   keep the nodes the are already visited, used no to iterate infinitely on graph cycles
      * @param q
      *   the next nodes to visit and their distance from `start`
      */
    def iter(visited: Set[ArticleId], q: Queue[(Int, ArticleId)]): WikiResult[Option[Int]] =
      q.dequeueOption match
        case None => WikiResult.successful(None)
        case Some(((currentDistance, currentArticle), articlesToSearch)) =>
          if currentArticle == target then WikiResult.successful(Some(currentDistance))
          else if currentDistance > maxDepth then WikiResult.successful(None)
          else
            val newVisited  = visited + currentArticle
            val newDistance = currentDistance + 1

            client
              .linksFrom(currentArticle)
              .map { neighborArticles =>
                val unvisitedNeighborEntries = neighborArticles
                  .filter(id => !newVisited.contains(id))
                  .map(id => (newDistance -> id))

                articlesToSearch.enqueueAll(unvisitedNeighborEntries)
              }
              .flatMap(toSearch => iter(newVisited, toSearch))
              .fallbackTo(iter(newVisited, articlesToSearch))

    if start == target then WikiResult.successful(Some(0))
    else iter(Set(start), Queue(0 -> start))

  /** Computes the distances between some pages provided the list of their titles. Do not compute the distance from page
    * and itself.
    *
    * @param titles
    *   names of the articles
    * @param maxDepth
    *   stop the search this value of distance is exceeded
    *
    * @return
    *   An asynchronous computation of the following form: Seq((distanceFromTitle, distanceToTitle, distance), ...)
    *
    * Hint: You should use the methods that you implemented on WikiResult as well as breadthFirstSearch
    */
  def distanceMatrix(titles: List[String], maxDepth: Int = 50): WikiResult[Seq[(String, String, Option[Int])]] =

    case class TitleAndId(title: String, id: ArticleId)

    def findTitlesAndIds(titles: List[String]): WikiResult[Seq[TitleAndId]] =
      WikiResult
        .traverse(titles) { title =>
          client
            .searchId(title)
            .map(TitleAndId(title, _))
        }

    def distinctPairs[A](as: Seq[A]): Seq[(A, A)] =
      for
        a1 <- as
        a2 <- as
        if a1 != a2
      yield (a1, a2)

    def distanceBetween(a: TitleAndId, b: TitleAndId): WikiResult[(String, String, Option[Int])] =
      breadthFirstSearch(a.id, b.id, maxDepth)
        .map(dist => (a.title, b.title, dist))

    findTitlesAndIds(titles)
      .map(distinctPairs)
      .flatMap(WikiResult.traverse(_)(distanceBetween.tupled))

end Wikigraph
