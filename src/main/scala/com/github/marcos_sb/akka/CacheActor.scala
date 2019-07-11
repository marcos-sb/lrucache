package com.github.marcos_sb.akka

import akka.actor.{Actor, ActorLogging, Props}
import com.github.marcos_sb.akka.CacheActor.{AddRequest, AddResponse, GetRequest, GetResponse}

object CacheActor {
  def props[K,V](): Props = Props[CacheActor[K,V]]

  final case class AddRequest[K,V](requestId: Long, key: K, value: V)
  final case class AddResponse[V](responseId: Long, value: V)
  final case class GetRequest[K](requestId: Long, key: K)
  final case class GetResponse[V](requestId: Long, value: V)
}

class CacheActor[K, V]() extends Actor with ActorLogging {
  var lastValue: V = _
  override def receive: Receive = {
    case AddRequest(id:Long, k:K, v:V) =>
      lastValue = v
      sender() ! AddResponse(id:Long, v:V)
    case GetRequest(id, k) => sender() ! GetResponse(id, lastValue)
  }
}
