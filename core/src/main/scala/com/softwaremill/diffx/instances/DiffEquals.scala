package com.softwaremill.diffx.instances

import com.softwaremill.diffx._

/** Diff where the two values are compared by using the equals method. If the two values aren't equal, then we use the
  * provided valueToString function to output the diagnostic output.
  */
final class DiffEquals[T](isIgnored: Boolean, valueToString: T => String) extends DiffValue[T] {

  def apply(left: T, right: T): DiffResult = {
    if (isIgnored) {
      DiffResult.Ignored
    } else {
      if (left != right) {
        DiffResultValue(valueToString(left), valueToString(right))
      } else {
        IdenticalValue(valueToString(left))
      }
    }
  }

  override def configureIgnored(newIgnored: Boolean): DiffEquals[T] =
    new DiffEquals[T](isIgnored = newIgnored, valueToString = valueToString)

  override def configurePath(
      step: String,
      nextPath: ConfigurePath,
      op: ConfigureOp
  ): Either[ConfigureError, DiffEquals[T]] = Left(ConfigureError.PathTooLong(nextPath))

  override def configurePairBy(path: ConfigurePath, op: ConfigureOp.PairBy[_]): Either[ConfigureError, DiffEquals[T]] =
    Left(ConfigureError.InvalidConfigureOp(path, op, "EqualsDiff"))

}
