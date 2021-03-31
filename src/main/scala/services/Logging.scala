package services

import zio.{Has, Task, ZIO, ZLayer}


object Logging {

  type LoggingEnv = Has[Logging.Service]

  /** the specification of the service */
  trait Service {
    def logError(message: String):  Task[Unit]
    def logInfo(message: String): Task[Unit]
  }

  /** the live implementation */
  val live:ZLayer[Any,Nothing, Has[Logging.Service]] = ZLayer.succeed(
    new Service {
      override def logError(message: String): Task[Unit] = Task {
        println(s"Error: $message")
      }

      override def logInfo(message: String): Task[Unit] = Task {
        println(s"Info: $message")
      }
    }
  )

  /** a clean high level method for accessing the service */
  def logError(message: String): ZIO[Has[Logging.Service], Throwable, Unit] = {
      ZIO.accessM(hasService => hasService.get.logError(message))
  }

  /** a clean high level method for accessing the service */
  def logInfo(message: String): ZIO[Has[Logging.Service], Throwable, Unit] = {
    ZIO.accessM(hasService => hasService.get.logInfo(message))
  }

}


