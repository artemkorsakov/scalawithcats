package com.github.artemkorsakov.tests.cats

import com.github.artemkorsakov.cats.{ TestUptimeClient, UptimeService }
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

class SectionEightTestSuite extends AnyFunSuiteLike with Matchers {
  test("8 Case Study: Testing Asynchronous Code") {
    val hosts    = Map("host1" -> 10, "host2" -> 6)
    val client   = new TestUptimeClient(hosts)
    val service  = new UptimeService(client)
    val actual   = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

}
