package java.net

import java.io.FileDescriptor
import java.io.IOException
import java.io.InterruptedIOException

class PlainDatagramSocketImpl extends DatagramSocketImpl {
  import PlainDatagramSocketImpl._

  fd = new FileDescriptor()

  private val bindToDevice = false
  private var ipaddress = Array[Byte](0, 0, 0, 0)
  private var ttl = 1
  // TODO : apache network system
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

  def bind(port : Int, addr : InetAddress) : Unit = {
    // TODO : Access controller stuff ?
    // TODO : platform stuff ???
    if (port != 0)
      localPort = port
    else
      localPort = ??? // TODO : platform stuff ???

    try {
      setOption(SO_BROADCAST, Boolean.box(true))
    } catch {
      case e : IOException =>
    }
  }

  def close() : Unit = {
    // TODO : synchronised ???
    if (fd.valid()) {
      try {
        // TODO: platform stuff ???
      } catch {
        case e : IOException =>
      }
      fd = new FileDescriptor()
    }
  }

  def create() : Unit = ??? // TODO : platform stuff

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

  def getTimeToLive() : Int = {
    if (/* TODO : platform stuff */ MULTICAST_TTL != 0)
      ttl
    else
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

  protected[net] def peek(sender : InetAddress) : Int = {
    if (isNativeConnected) {
      var storageArray = Array.fill[Byte](10)(0)
      val pack = new DatagramPacket(storageArray, storageArray.length)
      // TODO : platform stuff
      connectedPort
    } else {
      ??? // TODO : platform stuff
    }
  }

  def receive(pack : DatagramPacket) : Unit = {
    try {
      if (isNativeConnected) {
        // TODO : platform stuff
        updatePacketRcvAddress(pack)
      } else {
        // TODO ! platform stuff
      }
    } catch {
      case e : InterruptedIOException =>
        throw new SocketTimeoutException(e.getMessage())
    }
  }

  def send(pack : DatagramPacket) : Unit = {
    if (isNativeConnected) {
      // TODO : Platform Stuff
    } else {
      // TODO : Platform stuff 
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

  // TODO : deprecated setTTL

  override def connect(inetAddr : InetAddress, port : Int) : Unit = {
    // TODO : platform stuff

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
      // TODO : platform stuff
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
        // TODO : platform stuff
        updatePacketRcvAddress(pack)
      } else {
        // TODO : platform stuff
      }
    } catch {
      case e : InterruptedIOException => throw new SocketTimeoutException(e.toString())
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
