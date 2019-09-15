package com.github.marcos_sb

object ConcurrentBlockingLRUCache {
  def apply[K,V](capacity: Int, buckets: Int): ConcurrentBlockingLRUCache[K,V] = {
    new ConcurrentBlockingLRUCache(capacity, buckets)
  }
}

class ConcurrentBlockingLRUCache[K,V](val capacity: Int = 1, val buckets: Int = 1) {
  if (capacity < 1) throw new IllegalArgumentException("Capacity cannot be less than 1")
  if (buckets < 1) throw new IllegalArgumentException("Buckets cannot be less than 1")

  val singleCacheCapacity: Int = if (capacity/buckets == 0) 1 else capacity/buckets
  val shards: Array[BlockingLRUCache[K,V]] = Array.fill(buckets)(new BlockingLRUCache(singleCacheCapacity))

  def add(key: K, value: V): V = {
    val shard = math.abs(key.hashCode()) % shards.length
    val singleLRUCache = shards(shard)
    singleLRUCache.add(key, value)
  }

  def add(kv: (K,V)): V = add(kv._1, kv._2)

  def get(key: K): Option[V] = {
    val shard = math.abs(key.hashCode()) % shards.length
    val singleLRUCache = shards(shard)
    singleLRUCache.get(key)
  }

  override def toString: String = {
    shards.mkString("|")
  }
}
