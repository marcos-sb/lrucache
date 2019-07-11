package com.github.marcos_sb

import org.scalatest.FunSuite

class ConcurrentBlockingLRUCacheTest extends FunSuite {
  test("add one element and get it") {
    val cache: ConcurrentBlockingLRUCache[Int, String] = new ConcurrentBlockingLRUCache
    val elem = (0, "zero")
    cache.add(elem)
    val cachedElem = cache.get(elem._1).get
    assert(elem._2 == cachedElem)
  }

  test("get inexistent element") {
    val cache: ConcurrentBlockingLRUCache[Int, String] = new ConcurrentBlockingLRUCache
    val cachedElem = cache.get(0)
    assert(cachedElem == Option.empty)
  }

  test("add two elements, expire/remove oldest") {
    val cache: ConcurrentBlockingLRUCache[Int, String] = new ConcurrentBlockingLRUCache
    val elem0 = (0, "zero")
    val elem1 = (1, "one")
    cache.add(elem0)
    cache.add(elem1)
    assert(cache.get(0) == Option.empty)
    assert(cache.get(1).get == elem1._2)
  }

  test("capacity at least 1 or die") {
    assertThrows[IllegalArgumentException] {
      val cache: ConcurrentBlockingLRUCache[Int, String] = new ConcurrentBlockingLRUCache(0)
    }
  }

  test("buckets at least 1 or die") {
    assertThrows[IllegalArgumentException] {
      val cache: ConcurrentBlockingLRUCache[Int, String] = new ConcurrentBlockingLRUCache(buckets = 0)
    }
  }
}