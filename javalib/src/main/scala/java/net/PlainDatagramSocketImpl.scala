package java.net

import java.io.FileDescriptor
import java.io.IOException
import java.io.InterruptedIOException

import scala.scalanative.native._
import scala.scalanative.runtime.ByteArray
import scala.scalanative.posix.errno._
import scala.scalanative.posix.sys.socket
import scala.scalanative.posix.sys.socketOps._
import scala.scalanative.posix.netinet.in
import scala.scalanative.posix.netinet.inOps._
import scala.scalanative.posix.netinet.tcp
import scala.scalanative.posix.arpa.inet
import scala.scalanative.posix.netdb._
import scala.scalanative.posix.netdbOps._
import scala.scalanative.posix.sys.ioctl._
import scala.scalanative.posix.fcntl._
import scala.scalanative.posix.sys.select._
import scala.scalanative.posix.sys.selectOps._
import scala.scalanative.posix.unistd.{close => cClose}
import scala.scalanative.posix.arpa.inet

class PlainDatagramSocketImpl extends DatagramSocketImpl {
  import PlainDatagramSocketImpl._

  fd = new FileDescriptor()

  private val bindToDevice = false
  private var ipaddress = Array[Byte](0, 0, 0, 0)
  private var ttl = 1
  // TODO : volatile ???
  private var isNativeConnected : Boolean = _
  var receiveTimeout : Int = _
  val streaming : Boolean = true
  var shutdownInput : Boolean = _
  private var connectedAddress : InetAddress = _
  private var connectedPort = -1
  private var trafficClass : Int = _

  def this(fd : FileDescriptor, localPort : Int) = {
    this()
    this.fd = fd
    this.localPort = localPort
  }

  private def fetchLocalPort(family: Int): Option[Int] = {
    val len = stackalloc[socket.socklen_t]
    val portOpt = if (family == socket.AF_INET) {
      val sin = stackalloc[in.sockaddr_in]
      !len = sizeof[in.sockaddr_in].toUInt

      if (socket.getsockname(fd.fd, sin.cast[Ptr[socket.sockaddr]], len) == -1) {
        None
      } else {
        Some(sin.sin_port)
      }
    } else {
      val sin = stackalloc[in.sockaddr_in6]
      !len = sizeof[in.sockaddr_in6].toUInt

      if (socket.getsockname(fd.fd, sin.cast[Ptr[socket.sockaddr]], len) == -1) {
        None
      } else {
        Some(sin.sin6_port)
      }
    }

    portOpt.map(inet.ntohs(_).toInt)
  }

  def bind(port : Int, addr : InetAddress) : Unit = {
    val hints = stackalloc[addrinfo]
    val ret   = stackalloc[Ptr[addrinfo]]
    string.memset(hints.cast[Ptr[Byte]], 0, sizeof[addrinfo])
    hints.ai_family = socket.AF_UNSPEC
    hints.ai_flags = AI_NUMERICHOST
    hints.ai_socktype = socket.SOCK_STREAM

    Zone { implicit z =>
      val cIP = toCString(addr.getHostAddress)
      if (getaddrinfo(cIP, toCString(port.toString), hints, ret) != 0) {
        throw new BindException(
          "Couldn't resolve address: " + addr.getHostAddress)
      }
    }

    val bindRes = socket.bind(fd.fd, (!ret).ai_addr, (!ret).ai_addrlen)

    val family = (!ret).ai_family
    freeaddrinfo(!ret)

    if (bindRes < 0) {
      throw new BindException(
        "Couldn't bind to an address: " + addr.getHostAddress +
        " on port: " + port.toString)
    }

    localPort = fetchLocalPort(family).getOrElse {
      throw new BindException(
        "Couldn't bind to address: " + addr.getHostAddress + " on port: " + port)
    }
    try {
      //setOption(SO_BROADCAST, Boolean.box(true))
    } catch {
      case e : IOException =>
    }
  }

  def close() : Unit = {
    // TODO : synchronised ???
    if (fd.valid()) {
      try {
        cClose(fd.fd)
      } catch {
        case e : IOException =>
      }
      fd = new FileDescriptor()
    }
  }

  def create() : Unit = {
    val sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, 0)
    if (sock < 0) throw new IOException("Couldn't create a socket")
    fd = new FileDescriptor(sock)
  }

  override def finalize() : Unit = close()
  
  def getOption(optID : Int) : Object = optID match {
    case SocketOptions.SO_TIMEOUT => Int.box(receiveTimeout)
    case SocketOptions.IP_TOS => Int.box(trafficClass)
    case _ => {
      val result : Object = ??? // TODO : platform stuff
      if (optID == SocketOptions.IP_MULTICAST_IF &&
          (/* TODO : Platform stuff */ MULTICAST_IF) != 0) {
        try {
          InetAddress.getByAddress(ipaddress)
        } catch {
          case e : UnknownHostException => null
        }
      }
      result
    }
  }

  protected[net] def getTTL() : Byte = {
    Byte.box(getOption(IP_MULTICAST_TTL).asInstanceOf[Byte])
  }

  def getTimeToLive() : Int = {
    Byte.box(getOption(IP_MULTICAST_TTL).asInstanceOf[Byte]) & 0xFF
  }

  def join(addr : InetAddress) : Unit =
    setOption(IP_MULTICAST_ADD, ???) // TODO : platfrom stuff

  def joinGroup(addr : SocketAddress, netInterface : NetworkInterface) : Unit = {
    if (addr.isInstanceOf[InetSocketAddress]) {
      val groupAddr = addr.asInstanceOf[InetSocketAddress].getAddress
      setOption(IP_MULTICAST_ADD, ???) // TODO : platform stuff
    }
  }

  def leave(addr : InetAddress) : Unit =
    setOption(IP_MULTICAST_DROP, ???) // TODO : platform stuff

  def leaveGroup(addr : SocketAddress, netInterface : NetworkInterface) : Unit = {
    if (addr.isInstanceOf[InetSocketAddress]) {
      val groupAddr = addr.asInstanceOf[InetSocketAddress].getAddress
      setOption(IP_MULTICAST_DROP, ???) // TODO : platform stuff
    }
  }

  private def selectRead(fd : FileDescriptor, timeout: Int) : Unit = {
    if (timeout > 0) {
      val fdset = stackalloc[fd_set]
      !fdset._1 = stackalloc[CLongInt](FD_SETSIZE / (8 * sizeof[CLongInt]))
      FD_ZERO(fdset)
      FD_SET(fd.fd, fdset)

      val time = stackalloc[timeval]
      time.tv_sec = timeout / 1000
      time.tv_usec = (timeout % 1000) * 1000

      val selectRes = select(fd.fd + 1, fdset, null, null, time)
      selectRes match {
        case 0 =>
          throw new SocketTimeoutException(
            "Accept timed out, "
              + "SO_TIMEOUT was set to: " + timeout)
        case -1 => throw new SocketException(fromCString(string.strerror(errno.errno)))
        case _  => {}
      }
    }
  }

  private def recvConnectedDatagram(packet : DatagramPacket, peek : Boolean) : Int = {
    val localCount = if (packet.length < 65536) packet.length else 65536
    Zone { implicit z =>
      val message = alloc[Byte](localCount)
      var flags = 0
      if (!fd.valid()) {
        throw new SocketException("Bad Socket.")
      }
      selectRead(fd, receiveTimeout)

      if (peek) {
        flags = socket.MSG_PEEK
      }
      val res = socket.recv(fd.fd, message, localCount, flags).toInt

      if (res < 0 || errno.errno == ECONNREFUSED) {
        throw new PortUnreachableException(fromCString(string.strerror(errno.errno)))
      } else if (res < 0) {
        throw new SocketException(fromCString(string.strerror(errno.errno)))
      }

      packet.length = res.toInt

      if (res > 0) {
        for (i <- 0 until localCount) {
          packet.data(i) = !(message + i)
        }
      }
      res.toInt
    }
  }

  private def receiveDatagram(pack : DatagramPacket, peek : Boolean) : Int = {
    val len = stackalloc[socket.socklen_t]
    !len = sizeof[in.sockaddr_in6].toUInt
    val sin = stackalloc[socket.sockaddr]

    val localCount = if (pack.length < 65536) pack.length else 65536
    Zone { implicit z =>
      val message = alloc[Byte](localCount)
      if (!fd.valid()) {
        throw new SocketException("Bad Socket.")
      }
      selectRead(fd, receiveTimeout)
      var flags = 0
      if (peek) {
        flags = socket.MSG_PEEK
      }
      val result = socket.recvfrom(fd.fd, message, localCount, flags, sin, len).toInt

      pack.port = if (!sin._1 != socket.AF_INET.toUShort) {
        val addr4 = sin.cast[Ptr[in.sockaddr_in]]
        val addr4in = addr4.sin_addr.in_addr
        val addrBytes = Array.fill[Byte](4)(0)
        for (i <- 3 to 0 by -1) {
          addrBytes(i) = (addr4in >> i * 8).toByte
        }
        pack.setAddress(new Inet4Address(addrBytes))
        inet.ntohs(!addr4._2).toInt
      } else {
        val addr6 = sin.cast[Ptr[in.sockaddr_in6]]
        val addr6in = addr6.sin6_addr
        val addrBytes = Array.fill[Byte](16)(0)
        for (i <- 0 until 16) {
          addrBytes(i) = (!((addr6in._1)._1 + i)).toByte
        }
        pack.setAddress(new Inet6Address(addrBytes))
        inet.ntohs(!(addr6 + 2).cast[Ptr[in.in_port_t]]).toInt
      }

      if (result > 0) {
        for (i <- 0 until localCount) {
          pack.data(i) = !(message + i)
        }
      }
      result.toInt
    }
  }

  protected[net] def peek(sender : InetAddress) : Int = {
    if (isNativeConnected) {
      val storageArray = Array.fill[Byte](10)(0)
      val pack = new DatagramPacket(storageArray, storageArray.size)

      recvConnectedDatagram(pack, true)

      for (i <- 0 until connectedAddress.ipAddress.size) {
        sender.ipAddress(i) = connectedAddress.ipAddress(i)
      }
      connectedPort
    } else {
      val len = stackalloc[socket.socklen_t]
      !len = sizeof[in.sockaddr_in6].toUInt
      val sin = stackalloc[socket.sockaddr]

      Zone { implicit z =>
        val message = alloc[Byte](1)
        if (!fd.valid()) {
          throw new SocketException("Bad Socket.")
        }
        selectRead(fd, receiveTimeout)
        val result = socket.recvfrom(fd.fd, message, 1, socket.MSG_PEEK, sin, len).toInt

        if (result < 0) {
          throw new SocketException(fromCString(string.strerror(errno.errno)))
        }
      }

      val port = if (!sin._1 != socket.AF_INET.toUShort) {
        val addr4 = sin.cast[Ptr[in.sockaddr_in]]
        val addr4in = addr4.sin_addr.in_addr
        for (i <- 3 to 0 by -1) {
          sender.ipAddress(i) = (addr4in >> i * 8).toByte
        }
        inet.ntohs(!addr4._2)
      } else {
        val addr6 = sin.cast[Ptr[in.sockaddr_in6]]
        val addr6in = addr6.sin6_addr
        for (i <- 0 until 16) {
          sender.ipAddress(i) = (!((addr6in._1)._1 + i)).toByte
        }
        inet.ntohs(!(addr6 + 2).cast[Ptr[in.in_port_t]])
      }
      port.toInt
    }
  }

  def receive(pack : DatagramPacket) : Unit = {
    try {
      if (isNativeConnected) {
        recvConnectedDatagram(pack, false)
        updatePacketRcvAddress(pack)
      } else {
        receiveDatagram(pack, false)
      }
    } catch {
      case e : InterruptedIOException =>
        throw new SocketTimeoutException(e.getMessage())
    }
  }

  private def sendMsgConn(fd : FileDescriptor, msg : Ptr[Byte], sent : Int, length : Int, flags: Int) : Int = {
    if (!fd.valid()) {
      if (sent == 0) {
        throw new SocketException("Bad Socket.")
      } else {
        throw new InterruptedIOException("The call was cancelled.")
      }
    }
    val result = socket.send(fd.fd, (msg + sent), length, flags).toInt
    if (result < 0) {
      result
    } else {
      val newLength = length - result.toInt
      val newSent = sent + result.toInt
      if (newLength > 0)
        sendMsgConn(fd, msg, newSent, newLength, flags)
      else
        newSent
    }
  }

  private def sendMsg(fd : FileDescriptor, msg : Ptr[Byte], sent : Int, length : Int, flags: Int,
                      destAddr : Ptr[socket.sockaddr], addrlen : socket.socklen_t) : Int = {
    val result = socket.sendto(fd.fd, (msg + sent), length, flags, destAddr, addrlen).toInt
    if (result < 0) {
      result
    } else {
      val newLength = length - result.toInt
      val newSent = sent + result.toInt
      if (newLength > 0)
        sendMsg(fd, msg, newSent, newLength, flags, destAddr, addrlen).toInt
      else
        newSent
    }
  }

  def getSockAddr(address : InetAddress, port: Int) : Ptr[socket.sockaddr] = address match {
    case in4 : Inet4Address => {
      val in4addr = stdlib.malloc(sizeof[in.sockaddr_in]).cast[Ptr[in.sockaddr_in]]
      in4addr.sin_family = socket.AF_INET.toUShort
      in4addr.sin_port = inet.htons(port.toUShort)
      val in4addr_b = in4addr.sin_addr
      in4addr_b.in_addr = 0.toUInt
      for (i <- 0 until 4) {
        in4addr_b.in_addr = in4addr_b.in_addr | (in4.ipAddress(i).toUByte << (i * 8))
      }
      in4addr.cast[Ptr[socket.sockaddr]]
    }
    case in6 : Inet6Address => {
      val in6addr = stdlib.malloc(sizeof[in.sockaddr_in6]).cast[Ptr[in.sockaddr_in6]]
      in6addr.sin6_family = socket.AF_INET6.toUShort
      in6addr.sin6_port = inet.htons(port.toUShort)
      in6addr.sin6_flowinfo = ((trafficClass & 0xFF) << 20).toUShort
      val in6addr_b = in6addr.sin6_addr
      for (i <- 0 until 16) {
        !(in6addr_b._1._1 + i) = in6.ipAddress(i).toUByte
      }
      in6addr.sin6_scope_id = 0.toUInt
      in6addr.cast[Ptr[socket.sockaddr]]
    }
  }

  def send(pack : DatagramPacket) : Unit = {
    Zone { implicit z => 
      val message = alloc[Byte](pack.length)
      for (i <- 0 until pack.length) {
        !(message + i) = pack.data(i)
      }
      if (isNativeConnected) {
        val result = sendMsgConn(fd, message, 0, pack.length, 0)

        if (result < 0 || errno.errno == ECONNREFUSED) {
          throw new PortUnreachableException(fromCString(string.strerror(errno.errno)))
        } else if (result < 0) {
          throw new SocketException(fromCString(string.strerror(errno.errno)))
        }
      } else {
        val sockaddr : Ptr[socket.sockaddr] = getSockAddr(pack.address, pack.port)
        val addrLen = pack.address match {
          case in4 : Inet4Address => sizeof[in.sockaddr_in]
          case in6 : Inet6Address => sizeof[in.sockaddr_in6]
        }
        if (!fd.valid()) {
          throw new SocketException("Bad socket.")
        }
        val result = sendMsg(fd, message, 0, pack.length, 0, sockaddr, addrLen.toUInt)
        //stdlib.free(sockaddr.cast[Ptr[Byte]])

        if (result < 0) {
          throw new SocketException(fromCString(string.strerror(errno.errno)))
        }
        result
      }
    }
  }

  def setOption(optionID : Int, value : Object) : Unit = {
    var optID = optionID
    if (optID == SocketOptions.SO_REUSEADDR) {
      optID = REUSEADDR_AND_REUSEPORT
    }

    if (optID == SocketOptions.SO_TIMEOUT) {
      receiveTimeout = Int.unbox(value)
    } else {
      val flags : Int = ??? // TODO : platform stuff
      try {
        // TODO : platform stuff
      } catch {
        case e : SocketException =>
          if (optID != SocketOptions.IP_TOS) {
            throw e
          }
      }
      if (optID == SocketOptions.IP_MULTICAST_IF && (flags & MULTICAST_IF) != 0) {
        val inet = value.asInstanceOf[InetAddress]
        if (???){ // TODO : platform stuff
          ipaddress = value.asInstanceOf[InetAddress].getAddress()
        } else {
          var local : InetAddress = null
          try {
            local = InetAddress.getLocalHost()
          } catch {
            case e : UnknownHostException =>
              throw new SocketException("getLocalHost():" + e.toString())
          }
          if (inet == local)
            ipaddress = value.asInstanceOf[InetAddress].getAddress()
          else
            throw new SocketException(value + " != getLocalHost() : " + local)
        }
      }
      if (optID == SocketOptions.IP_TOS) {
        trafficClass = Int.unbox(value)
      }
    }
  }

  protected[net] def setTimeToLive(ttl : Int) : Unit = {
    setOption(IP_MULTICAST_TTL, Byte.box((ttl & 0xFF).asInstanceOf[Byte])) // TODO : correct way to cast ??
    if (MULTICAST_TTL != 0){ // TODO : Platform stuff
      this.ttl = ttl
    }
  }

  protected[net] def setTTL(ttl : Byte) : Unit = {
    setOption(IP_MULTICAST_TTL, Byte.box(ttl))
    if (MULTICAST_TTL != 0) { // TODO : Platform stuff
      this.ttl = ttl
    }
  }

  override def connect(inetAddr : InetAddress, port : Int) : Unit = {
    if (!fd.valid()) {
      throw new SocketException("Bad Socket.")
    }
    val sockAddr = getSockAddr(inetAddr, port)
    val addrLen = inetAddr match {
      case in4 : Inet4Address => sizeof[in.sockaddr_in].toUInt
      case in6 : Inet6Address => sizeof[in.sockaddr_in].toUInt
    }
    val result = socket.connect(fd.fd, sockAddr, addrLen)
    if (result != 0) {
      throw new ConnectException(fromCString(string.strerror(errno.errno)))
    }

    try {
      connectedAddress = InetAddress.getByAddress(inetAddr.getAddress)
    } catch {
      case e : UnknownHostException =>
        val hostName = inetAddr.getHostName()
        throw new SocketException(s"Host is unresolved\: $hostName")
    }
    connectedPort = port
    isNativeConnected = true
  }

  override def disconnect() : Unit = {
    try {
      if (!fd.valid()) {
        throw new SocketException("Bad Socket.")
      }
      val result = Zone { implicit z => 
        val sockAddr = alloc[socket.sockaddr]
        !sockAddr._1 = socket.AF_UNSPEC.toUShort
        val addrLen = sizeof[socket.sockaddr].toUInt
        socket.connect(fd.fd, sockAddr, addrLen)
      }
      if (result != 0) {
        throw new SocketException(fromCString(string.strerror(errno.errno)))
      }
    } catch {
      case e : Exception => // eat exception
    }
    connectedPort = -1
    connectedAddress = null
    isNativeConnected = false
  }

  def peekData(pack : DatagramPacket) : Int = {
    try {
      if (isNativeConnected) {
        recvConnectedDatagram(pack, true)
        updatePacketRcvAddress(pack)
      } else {
        receiveDatagram(pack, true)
      }
    } catch {
      case e : InterruptedIOException =>
        throw new SocketTimeoutException(e.getMessage())
    }
    pack.getPort()
  }
  
  private def updatePacketRcvAddress(packet : DatagramPacket) : Unit = {
    packet.setAddress(connectedAddress)
    packet.setPort(connectedPort)
  }
}

object PlainDatagramSocketImpl {
  private[net] final val MULTICAST_IF = 1
  private[net] final val MULTICAST_TTL = 2
  private[net] final val TCP_NODELAY = 4
  private[net] final val FLAG_SHUTDOWN = 8
  private final val SO_BROADCAST = 32
  private[net] final val IP_MULTICAST_ADD = 19
  private[net] final val IP_MULTICAST_DROP = 20
  private[net] final val IP_MULTICAST_TTL = 17
  private[net] final val REUSEADDR_AND_REUSEPORT = 1001
}
