package scala.scalanative
package native

import scalanative.posix.sys.socket

@extern
object ifaddrs {
  type ifaddrs = CStruct7[Ptr[Byte], // ifa_next
                          CString, // ifa_name
                          CUnsignedInt, // ifa_flags
                          Ptr[socket.sockaddr], //ifa_addr
                          Ptr[socket.sockaddr], //ifa_netmask
                          Ptr[socket.sockaddr], //ifu_broadaddr
                          Ptr[Byte]] // ifa_data

  def getifaddrs(ifap : Ptr[Ptr[ifaddrs]]) : CInt = extern
  def freeifaddrs(ifa : Ptr[ifaddrs]) : Unit = extern
}

