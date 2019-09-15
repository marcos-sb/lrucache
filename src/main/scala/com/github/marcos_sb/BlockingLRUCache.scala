package com.github.marcos_sb

import scala.collection.mutable

object BlockingLRUCache {
  def apply[K,V](capacity: Int) = new BlockingLRUCache[K,V](capacity)
}

class BlockingLRUCache[K,V](val capacity: Int) {
  case class Node(key: K, value: V) {
    var prev, next: Node = _
    override def toString: String = {
      val sb = new StringBuilder()
      if (prev != null) sb.append("<-")
      sb.append(s"[${value.toString}]")
      if (next != null) sb.append("->")
      sb.mkString
    }
  }

  var head, last: Node = _
  var currentSize = 0
  val cache: mutable.Map[K, Node] = mutable.Map[K, Node]()

  def add(key: K, value: V): V = this.synchronized {
    if (currentSize == capacity) {
      val oldest = head
      head = oldest.next
      oldest.next = null
      if (head != null) head.prev = null
      else last = null
      cache -= oldest.key
      currentSize -= 1
    }
    val newNode = Node(key, value)
    cache += key -> newNode
    currentSize += 1
    if (last != null) last.next = newNode
    newNode.prev = last
    last = newNode
    if (head == null) head = newNode
    value
  }

  def get(key: K): Option[V] = this.synchronized {
    val maybeNode = cache.get(key)
    maybeNode match {
      case Some(node) =>
        if (node == last) return Some(node.value)
        if (node.prev != null) node.prev.next = node.next
        if (node.next != null) node.next.prev = node.prev
        if (head == node) head = node.next
        if (last != null) last.next = node
        last = node
        node.prev = last
        node.next = null
        Some(node.value)
      case _ => None
    }
  }

  override def toString: String = {
    val sb = new StringBuilder()
    var currentNode = head
    while (currentNode != null) {
      sb.append(currentNode.toString)
      currentNode = currentNode.next
    }
    sb.mkString
  }
}
