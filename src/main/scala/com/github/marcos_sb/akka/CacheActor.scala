package com.github.marcos_sb.akka

import akka.actor.{Actor, ActorLogging, Props}
import com.github.marcos_sb.akka.CacheActor.{AddRequest, AddResponse, GetRequest, GetResponse}

object CacheActor {
  def props[K,V](capacity: Int): Props = Props(new CacheActor[K,V](capacity))

  final case class AddRequest[K,V](requestId: Long, key: K, value: V)
  final case class AddResponse[V](responseId: Long, value: V)
  final case class GetRequest[K](requestId: Long, key: K)
  final case class GetResponse[V](requestId: Long, value: Option[V])
}

class CacheActor[K, V](capacity: Int) extends Actor with ActorLogging {
  val cache: LRUCache[K,V] = LRUCache(capacity)
  override def receive: Receive = {
    case AddRequest(id:Long, k:K, v:V) =>
      cache.add(k, v)
      sender() ! AddResponse(id:Long, v:V)
    case GetRequest(id: Long, k: K) =>
      sender() ! GetResponse(id, cache.get(k))
  }
}
