package cyclo

import spire.util.Opt

/** Computes the number-theoretical properties of the order of a cyclotomic.  */
object OrderProperties { 

  // Precomputed table for orders 1..15
  object Table {
    val t = true
    val f = false
    //                          0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
    val phi            = Array(-1, 1, 1, 2, 2, 4, 2, 6, 4, 6, 4,10, 4,12, 6, 8)
    val squareFree     = Array( t, t, t, t, f, t, t, t, f, f, t, t, f, t, t, t)
    val numberOfPrimes = Array(-1, 0, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 2, 1, 2, 2)
  }

  val lock = new java.util.concurrent.locks.ReentrantLock

  @volatile private[this] var order: Int = 16
  @volatile private[this] var phi = 8
  @volatile private[this] var squareFree = false
  @volatile private[this] var numberOfPrimes = 1

  protected def fromCache(c: WorkCyclo): Unit = {
    val n = c.order
    c.phi = Table.phi(n)
    c.squareFree = Opt(Table.squareFree(n))
    c.numberOfPrimes = Table.numberOfPrimes(n)
  }

  protected def compute(c: WorkCyclo): Unit = {
    val n = c.order
    var phi = n
    var k = n
    var isSqFree = true
    var nrp = 0
    var p = 2
    while (p <= k) {
      if (k % p == 0) {
        phi = (phi / p) * (p - 1)
        if ( k % (p * p) == 0) isSqFree = false // detect presence of square
        nrp += 1
        k /= p
        while ( k % p == 0) k /= p
      }
      p += 1
    }
    c.phi = phi
    c.squareFree = Opt(isSqFree)
    c.numberOfPrimes = nrp
  }

  def apply(c: WorkCyclo): Unit =
    if (c.phi == -1 || c.squareFree.isEmpty || c.numberOfPrimes == 1) {// something missing
      if (c.order < 16) fromCache(c) else {
        if (lock.tryLock()) {
          try {
            if (order == c.order) {
              c.phi = phi
              c.squareFree = Opt(squareFree)
              c.numberOfPrimes = numberOfPrimes
            } else {
              compute(c)
              // To prevent invalid cache state if thread is killed by Thread.stop,
              // we write over order first
              order = -1
              phi = c.phi
              squareFree = c.squareFree.get
              numberOfPrimes = c.numberOfPrimes
              order = c.order
            }
          } finally {
            lock.unlock()
          }
        } else compute(c) // if lock cannot be acquired
      }
    }

}
