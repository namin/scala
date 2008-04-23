
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

// generated by genprod on Wed Apr 23 10:06:16 CEST 2008  (with extra methods)

package scala


/** <p>
 *    Function with 14 parameters.
 *  </p>
 *
 */
trait Function14[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, +R] extends AnyRef { self =>
  def apply(v1:T1, v2:T2, v3:T3, v4:T4, v5:T5, v6:T6, v7:T7, v8:T8, v9:T9, v10:T10, v11:T11, v12:T12, v13:T13, v14:T14): R
  override def toString() = "<function>"

  /** f(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14)  == (f.curry)(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x10)(x11)(x12)(x13)(x14)
   */
  def curry: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => R = {
      (x1: T1) => ((x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14) => self.apply(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14)).curry
  }
}
