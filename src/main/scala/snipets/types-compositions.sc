import java.net.{Socket, SocketAddress}

type SocketFactory = SocketAddress => Socket
val addrToInet: SocketAddress => Long
val inetToSocket: Long => Socket

val factory: SocketFactory = addrToInet andThen inetToSocket
