package java.net

import scala.collection.mutable.MutableList
import collection.JavaConverters._

import java.util.Enumeration

final class NetworkInterface private (
    name : String,
    displayName : String,
    interfaceIndex : Int) extends Object {

  private val addresses : MutableList[InetAddress] = new MutableList()
  private var parent : NetworkInterface = null
  private val children : MutableList[NetworkInterface] = new MutableList()
  private val hash = name.hashCode()
  
  def this(name : String, displayName : String, addresses : Array[InetAddress], interfaceIndex : Int) = {
    this(name, displayName, interfaceIndex)
    
    if (addresses != null) {
      this.addresses ++= addresses
    }
  }

  private[net] def getIndex() : Int = interfaceIndex

  private[net] def getFirstAddress() : InetAddress = {
    if (addresses.size >= 1)
      addresses.head
    else
      null
  }

  private[net] def getName() : String = name

  def getInetAddresses() : Enumeration[InetAddress] = addresses.iterator.asJavaEnumeration

  private def getFullFormOfCompressesIPV6Address(compressedArg : String) = {
    var compressed = compressedArg
    val fullForm = new StringBuilder(39)
    val NUM_OF_IPV6_FIELDS = 8

    var fields = compressed.split(":")

    var numOfCompressedFields : Int = 0

    if (compressed.startsWith("::")) {
      compressed = compressed.replace("::", "")
      fields = compressed.split(":")
      numOfCompressedFields = NUM_OF_IPV6_FIELDS - fields.size
      for (i <- 0 until numOfCompressedFields) {fullForm ++= "0:"}
      fields.foreach(fullForm ++= _ + ":")
    } else if (compressed.endsWith(":")) {
      compressed = compressed.replace("::", "")
      fields = compressed.split(":")
      numOfCompressedFields = NUM_OF_IPV6_FIELDS - fields.size
      fields.foreach(fullForm ++= _ + ":")
      for (i <- 0 until numOfCompressedFields) {fullForm ++= "0:"}
    } else {
      numOfCompressedFields = NUM_OF_IPV6_FIELDS - fields.size + 1
      fields.foreach((field : String) => field match {
        case "" => for (i <- 0 until numOfCompressedFields) {fullForm ++= "0:"}
        case _ =>
          fullForm ++= field + ":"
      })
    }

    fullForm.dropRight(1).toString()
  }

  def getDisplayName() : String = displayName match {
    case "" => name
    case _ => displayName
  }

  override def equals(obj : Any) : Boolean = obj match {
    case obj : AnyRef if this eq obj => true
    case netif : NetworkInterface => {
      ((name != "" && netif.getName() == name) ||
       (name == "" && netif.getName() == displayName)) && 
      netif.addresses == addresses
    }
    case _ => false
  }

  override def hashCode() : Int = hash

  override def toString() : String = {
    val string = new StringBuilder(25)
    string += '[' ++= name ++= "][" ++= displayName += ']'
    addresses.foreach(addr => string += '[' ++= addr.toString() += ']')

    string.toString()
  }

  // TODO : this actually calls only the native static method
  def getInterfaceAddresses() : List[InterfaceAddress] = ???

  def getSubInterfaces() : Enumeration[NetworkInterface] = children.iterator.asJavaEnumeration

  def getParent() : NetworkInterface = parent

  def isUp() : Boolean = {
    if (addresses.size == 0) {
      false
    } else {
      // TODO : call to native method
      // call to isUpImpl(name, interfaceIndex)
      // call to getPlatformIsUp(name, index)
      // call getPlatformNetworkInterfaceAttribute(name, IFF_UP)
      ???
    }
  }

  def isLoopback() : Boolean = {
    if (addresses.size == 0) {
      false
    } else {
      // TODO : call to native method
      ???
    }
  }

  def isPointToPoint() : Boolean = {
    if (addresses.size == 0) {
      false
    } else {
      // TODO : call to native method
      ???
    }
  }

  def supportsMulticast() : Boolean = {
    if (addresses.size == 0) {
      false
    } else {
      // TODO : call to native method
      ???
    }
  }

  def getHardwareAddress() : Array[Byte] = {
    if (addresses.size == 0) {
      Array[Byte]()
    } else {
      // TODO : call to native method
      ???
    }
  }

  def getMTU() : Int = {
    if (addresses.size == 0) {
      0
    } else {
      // TODO : call to native method
      ???
    }
  }

  def isVirtual() : Boolean = parent != null
}

object NetworkInterface {
  // TODO : native
  private def getNetworkInterfacesImpl() : Array[NetworkInterface] = ???

  def getByName(interfaceName : String) : NetworkInterface = {
    if (interfaceName == null) {
      throw new NullPointerException("interface name is null")
    }

    val interfaces = getNetworkInterfaces().asScala
    if (interfaces != null)
      interfaces.find(x => x.getName() == interfaceName) match {
        case Some(interface) => interface
        case None => null
      }
    else
      null
  }

  def getByInetAddress(address : InetAddress) : NetworkInterface = {
    if (address == null) {
      throw new NullPointerException("address is null")
    }

    val interfaces = getNetworkInterfaces().asScala
    if (interfaces != null)
      interfaces.find(x => x.addresses.exists(y => y == address)) match {
        case Some(interface) => interface
        case None => null
      }
    else
      null
  }

  def getNetworkInterfaces() : Enumeration[NetworkInterface] = {
    val interfaces = getNetworkInterfacesImpl()
    if (interfaces == null) {
      null
    } else {
      val peeked = Array.fill[Boolean](interfaces.size)(false)

      // TODO : check if these fields are good
      interfaces.foreach(netif => netif.addresses.foreach(addr => {
        if (addr.isLinkLocalAddress() || addr.isSiteLocalAddress()) {
          addr.asInstanceOf[Inet6Address].scopedIf = netif
          addr.asInstanceOf[Inet6Address].ifname = netif.getName()
          addr.asInstanceOf[Inet6Address].scope_ifname_set = true
        }
      }))

      val hlist = List[NetworkInterface]()

      val netifMap = interfaces.map(netif => (netif.getName(), netif)).toMap
      interfaces.foreach(netif => netif.getName().split(":") match {
        case Array(x, _, _*) if netifMap.contains(x) => {
          netifMap(x).children += netif
          netif.parent = netifMap(x)
          netifMap(x).addresses ++= netif.addresses
        }
        case _ => // Do nothing
      })

      hlist.iterator.asJavaEnumeration
    }
  }
}
