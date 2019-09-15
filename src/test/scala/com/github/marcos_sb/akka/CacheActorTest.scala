package com.github.marcos_sb.akka

import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit}

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import com.github.marcos_sb.akka.CacheActor.{AddRequest, GetRequest}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class CacheActorTest
  extends TestKit(ActorSystem("MySpec"))
  with ImplicitSender
  with WordSpecLike
  with Matchers
  with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A Cache Actor" must {
    "Respond with" in {
      val cacheActor = system.actorOf(CacheActor.props[Int, Int](1), "cache")
      val es = ExecutionContext.fromExecutorService(
        new ScheduledThreadPoolExecutor(40))
      val start = System.nanoTime()
      val s = for (i <- 1 to 10000) yield Future {
        cacheActor ! AddRequest(i, i, i)
      } (es)
      s.foreach(Await.ready(_,Duration.create(10,TimeUnit.SECONDS)))
      Await.ready(
        Future {cacheActor ! GetRequest(1,10000)}(es),
          Duration.create(10, TimeUnit.SECONDS))
      val end = System.nanoTime() - start
      println(s"${end / 1000000} ms")
    }
  }
}
