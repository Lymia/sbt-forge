package moe.lymia.sbt

import scala.collection.mutable.HashMap

package object forge {
  def cacheFunction[A, B](fn: A => B) = {
    val cache = new HashMap[A, B]
    (a: A) => cache.get(a).getOrElse {
      val v = fn(a)
      cache.put(a, v)
      v
    }
  }
}
