package underscore.cats.ch08

import cats.Applicative
import cats.implicits._ // for map

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  val ev = implicitly[Applicative[F]]
  def getTotalUptime(hostnames: List[String]): F[Int] = {
    val v = hostnames.traverse(client.getUptime)
    ev.map(v)(_.sum)
  }
}
