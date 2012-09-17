/*
  ---------------------------------------------------------------------------
  This software is released under a BSD license, adapted from
  http://opensource.org/licenses/bsd-license.php

  Copyright (c) 2010, Brian M. Clapper
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

   * Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

   * Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

   * Neither the names "clapper.org", "Grizzled Scala Library", nor the
    names of its contributors may be used to endorse or promote products
    derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------
*/

package grizzled

import scala.util.continuations._

/** Functions that can be used to simulate Python-style generators.
  * Adapted liberally from Rich Dougherty's solution, as outlined in
  * Stack Overflow: [[http://stackoverflow.com/questions/2201882#2215182]]
  *
  * Example usage:
  *
  * {{{
  * import grizzled.generator._
  * import scala.util.continuations._
  * import java.io.File
  *
  * def recursivelyListFiles(dir: File): Iterator[File] = generator[File] {
  *   def handleList(list: List[File]): Unit @cps[GeneratorIteration[File]] = {
  *     list match {
  *       case Nil => ()
  *
  *       case f :: tail => {
  *         generate(f)
  *         doList(if (f.isDirectory) f.listFiles.toList else Nil)
  *         doList(tail)
  *       }
  *     }
  *   }
  *
  *   handleList(dir.listFiles.toList)
  * }
  * }}}
  *
  * This package uses the Scala compilers continuations plug-in. The above
  * example must be compiled with tha plug-in enabled. Use the
  * `-P:continuations:enable` flag.
  */

class Generator[T] extends Iterator[T] with (T => Unit @suspendable) {
  private var a: Option[T] = None
  private var k: Option[Unit => Unit] = None

  def next = {
    val a2 = a.get
    val k2 = k.get
    a = None
    k = None
    k2()
    a2
  }

  def hasNext = k.isDefined

  def apply(a2: T): Unit @suspendable = {
    a = Some(a2)
    shift { k2: (Unit => Unit) =>
      k = Some(k2)
    }
  }
}

object generator {

  def generator[T](f: (T => Unit @suspendable) => Unit @suspendable): Iterator[T] = {
    val g = new Generator[T]
    reset { f(g) }
    g
  }

  trait SuspendableForeach[T] {
    def foreach(f: T => Unit @suspendable): Unit @suspendable
  }

  def suspendable[T](iter: Iterable[T]) = new SuspendableForeach[T] {
    def foreach(f: T => Unit @suspendable): Unit @suspendable = {
      val i = iter.iterator
      while (i.hasNext)
        f(i.next)
    }
  }
}
