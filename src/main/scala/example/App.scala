package example

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._

import scala.io.StdIn

object WebServer {

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  def main(args: Array[String]) {

    val route: Route =
      path("price") {
        get {
          parameters('books.as[String]) {
            books => complete(PotterBook.price(getAsIntList(books)).toString)
          }
        }
      }

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }

  private def getAsIntList(books: String) = {
    books.split(",").toList.map(_.trim.toInt)
  }

}