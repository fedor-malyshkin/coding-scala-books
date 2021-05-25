package underscore.cats.ch08

import scala.concurrent.Future

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}
trait RealUptimeClient extends UptimeClient[Future] {}
