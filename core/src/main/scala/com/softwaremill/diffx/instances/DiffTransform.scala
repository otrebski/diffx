package com.softwaremill.diffx.instances

import com.softwaremill.diffx._

/** A Diff that transforms any input of diff method and pass it to its underlying Diff. See [[ValueDiff.contramap]]
  */
class DiffTransformed[T, U](underlyingDiff: DiffValue[U], transformFunc: T => U) extends DiffValue[T] {

  def apply(left: T, right: T): DiffResult = {
    underlyingDiff.apply(transformFunc(left), transformFunc(right))
  }

  override def configureIgnored(newIgnored: Boolean): DiffTransformed[T, U] = {
    new DiffTransformed[T, U](
      underlyingDiff.configureIgnored(newIgnored),
      transformFunc
    )
  }

  override def configurePath(
      step: String,
      nextPath: ConfigurePath,
      op: ConfigureOp
  ): Either[ConfigureError, DiffTransformed[T, U]] = {
    underlyingDiff.configurePath(step, nextPath, op).map { newUnderlyingDiff =>
      new DiffTransformed(
        newUnderlyingDiff,
        transformFunc
      )
    }
  }

  override def configurePairBy(
      path: ConfigurePath,
      op: ConfigureOp.PairBy[_]
  ): Either[ConfigureError, DiffTransformed[T, U]] = {
    underlyingDiff.configurePairBy(path, op).map { newUnderlyingDiff =>
      new DiffTransformed(
        newUnderlyingDiff,
        transformFunc
      )
    }
  }
}
