package scala.scalanative
package posix
package net

import scalanative.native._

@extern
object _if {
  type ifconf = CStruct2[CInt,
                         CString]

  def if_nametoindex(ifname : CString) : UInt = extern

  @name("scalanative_IFF_RUNNING")
  def IFF_RUNNING: CInt = extern

  @name("scalanative_IFF_LOOPBACK")
  def IFF_LOOPBACK : CInt = extern

  @name("scalanative_IFF_POINTOPOINT")
  def IFF_POINTOPOINT : CInt = extern

  @name("scalanative_IFF_MULTICAST")
  def IFF_MULTICAST : CInt = extern
}

object ifOps {
  import _if._

  implicit class ifOps(val ptr: Ptr[ifconf]) extends AnyVal {
    def if_index: CInt = !ptr._1
    def if_name: CString = !ptr._2

    def if_index_=(v: CInt) = !ptr._1 = v
    def if_name_=(v: CString) = !ptr._2 = v
  }
}
