package scala.scalanative
package posix
package net

import scalanative.native._

@extern
object _if {
  type ifconf = CStruct2[CInt,
                         Ptr[Byte]]

  @name("scalanative_IFF_RUNNING")
  def IFF_RUNNING: CInt = extern

  @name("scalanative_IFF_LOOPBACK")
  def IFF_LOOPBACK : CInt = extern

  @name("scalanative_IFF_POINTOPOINT")
  def IFF_POINTOPOINT : CInt = extern

  @name("scalanative_IFF_MULTICAST")
  def IFF_MULTICAST : CInt = extern
}
