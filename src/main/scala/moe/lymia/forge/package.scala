package moe.lymia

import scala.collection.mutable

package object forge {
  def cacheFunction[A, B](fn: A => B) = {
    val cache = new mutable.HashMap[A, B]
    a: A => cache.getOrElse(a, {
      val v = fn(a)
      cache.put(a, v)
      v
    })
  }
}
