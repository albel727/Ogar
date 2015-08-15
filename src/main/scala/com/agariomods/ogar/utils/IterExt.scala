package com.agariomods.ogar.utils

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

class IterExt[T](iter : TraversableOnce[T]) {
  def getTop[Q](
                 n : Int,
                 f: (Boolean) => (T) => Q,
                 lb : ArrayBuffer[T] = new ArrayBuffer[T]()
                 )(implicit ord: Ordering[Q]) = IterExt.getTop[T, Q](n, iter, f, lb)(ord)
}

object IterExt {
  implicit def iterToIterExt[T](iter : TraversableOnce[T]) : IterExt[T] = new IterExt[T](iter)

  private final def insertOne[T, Q](player : T, leaderboard : ArrayBuffer[T], f: T => Q)(implicit ord: Ordering[Q]) {
    // Adds the player and sorts the leaderboard
    var len = leaderboard.size - 1
    var loop = true
    while ((len >= 0) && loop) {
      // Start from the bottom of the leaderboard
      if(ord.gteq(f(player), f(leaderboard(len)))) {
        leaderboard.insert(len + 1, player)
        loop = false // End the loop if a spot is found
      }
      len -= 1
    }
    if(loop) {
      // Add to top of the list because no spots were found
      leaderboard.insert(0, player)
    }
  }

  final def getTop[T, Q](n : Int, iter : TraversableOnce[T], f: (Boolean) => (T) => Q, result : ArrayBuffer[T] = new ArrayBuffer[T]())(implicit ord: Ordering[Q]) : ArrayBuffer[T] = {
    if(result.size > n) {
      result.remove(n, result.size - n)
    }

    iter.foreach(player => {
      val playerScore = f(true)(player)
      if(result.length >= n && ord.gt(playerScore, f(false)(result(n - 1)))) {
        result.remove(n - 1)
      }

      insertOne(player, result, f(false))
    })

    result
  }
}
