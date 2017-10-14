package java.net

import java.io.FileDescriptor

abstract class DatagramSocketImpl extends SocketOptions {
  protected[net] var fd : FileDescriptor = null
  protected[net] var localPort : Int = -1

  protected[net] def bind(port : Int, addr : InetAddress) : Unit

  protected[net] def close() : Unit

  protected[net] def create() : Unit

  protected[net] def getFileDescriptor() : FileDescriptor = fd

  // TODO : Apache harmony platform
  private[net] def getLocalAddress() : InetAddress = ???

  protected[net] def getLocalPort() : Int = localPort

  // TODO : Include deprecated method ?

  protected[net] def getTimeToLive() : Int

  protected[net] def join(addr : InetAddress) : Unit

  protected[net] def joinGroup(addr : SocketAddress, netInterface : NetworkInterface) : Unit

  protected[net] def leave(addr : InetAddress) : Unit

  protected[net] def leaveGroup(addr : SocketAddress, netInterface : NetworkInterface) : Unit

  protected[net] def peek(sender : InetAddress) : Int

  protected[net] def receive(pack : DatagramPacket) : Unit

  protected[net] def send(pack : DatagramPacket) : Unit

  protected[net] def setTimeToLive(ttl : Int) : Unit

  // TODO: Deprecated ??

  protected[net] def connect(inetAddr : InetAddress, port : Int) : Unit = { /* Do nothing */ }

  protected[net] def disconnect() : Unit = { /* Do nothing */ }

  protected[net] def peekData(pack : DatagramPacket) : Int
}
