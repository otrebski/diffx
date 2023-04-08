package com.softwaremill.diffx.instances

import com.softwaremill.diffx._

/** Diff where the error diagnostic output is just string values. Simple types where a string representation is enough
  * for diagnostics purposes should use Diff.useEquals (EqualsDiff is a subtype of this trait). For example, Diff for
  * Int, String, java.time.Instant are all ValueDiffs.
  *
  * This trait also provide an extra `contramap` method which makes it easy to write instances for newtypes / opaque
  * types.
  */
trait DiffValue[T] extends Diff[T] {

  override def configureIgnored(newIgnored: Boolean): DiffValue[T]

  override def configurePath(
      step: String,
      nextPath: ConfigurePath,
      op: ConfigureOp
  ): Either[ConfigureError, DiffValue[T]]

  override def configurePairBy(
      path: ConfigurePath,
      op: ConfigureOp.PairBy[_]
  ): Either[ConfigureError, DiffValue[T]]

  final def contramap[S](transformFunc: S => T): DiffTransformed[S, T] = {
    new DiffTransformed(this, transformFunc)
  }
}
