package com.softwaremill.diffx

import com.softwaremill.diffx.ObjectMatcher.{MapEntry, SeqEntry, SetEntry}
import com.softwaremill.diffx.instances._

trait Diff[T] extends DiffMacro[T] {
  def apply(left: T, right: T): DiffResult

  /** Attempt to change the configuration of this Diff. If successful, a new differ with the updated configuration will
    * be returned.
    *
    * The configuration change can fail due to
    *   - bad "path" that does not match the internal structure of the Diff
    *   - The path resolved correctly, but the configuration update operation cannot be applied for that part of the
    *     Diff (e.g. wrong type or wrong operation)
    *
    * @param path
    *   The path to traverse to the sub-Diff
    * @param operation
    *   The configuration change operation you want to perform on the target sub-Diff
    */
  final def configureRaw(path: ConfigurePath, operation: ConfigureOp): Either[ConfigureError, Diff[T]] = {
    (path.unresolvedSteps, operation) match {
      case (step :: tail, op) => configurePath(step, ConfigurePath(path.resolvedSteps :+ step, tail), op)
      case (Nil, ConfigureOp.SetIgnored(newIgnored)) => Right(configureIgnored(newIgnored))
      case (Nil, pairByOp: ConfigureOp.PairBy[_])    => configurePairBy(path, pairByOp)
      case (Nil, op: ConfigureOp.TransformDiff[_])   => Right(configureTransform(op))
    }
  }

  protected def configureIgnored(newIgnored: Boolean): Diff[T]

  protected def configurePath(step: String, nextPath: ConfigurePath, op: ConfigureOp): Either[ConfigureError, Diff[T]]

  protected def configurePairBy(path: ConfigurePath, op: ConfigureOp.PairBy[_]): Either[ConfigureError, Diff[T]]

  final private def configureTransform(
      op: ConfigureOp.TransformDiff[_]
  ): Diff[T] = {
    op.unsafeCastFunc[T].apply(this)
  }
}

object Diff extends LowPriorityDiff with DiffTupleInstances with DiffxPlatformExtensions with DiffCompanionMacro {
  def apply[T: Diff]: Diff[T] = implicitly[Diff[T]]

  // def ignored[T]: Diff[T] = (_: T, _: T, _: DiffContext) => DiffResult.Ignored

  def compare[T: Diff](left: T, right: T): DiffResult = apply[T].apply(left, right)

  /** Create a Diff instance using [[Object#equals]] */
  def useEquals[T]: Diff[T] = new DiffEquals[T](isIgnored = false, _.toString)

  def approximate[T: Numeric](epsilon: T): Diff[T] =
    new ApproximateDiffForNumeric[T](epsilon)

  implicit val diffForString: Diff[String] = new DiffForString
  implicit val diffForRange: Diff[Range] = Diff.useEquals[Range]
  implicit val diffForChar: Diff[Char] = Diff.useEquals[Char]
  implicit val diffForBoolean: Diff[Boolean] = Diff.useEquals[Boolean]

  implicit def diffForNumeric[T: Numeric]: Diff[T] = new DiffForNumeric[T]

  implicit def diffForMap[C[_, _], K, V](implicit
      dv: Diff[V],
      dk: Diff[K],
      matcher: ObjectMatcher[MapEntry[K, V]],
      mapLike: MapLike[C]
  ): Diff[C[K, V]] = new DiffForMap[C, K, V](matcher, dk, dv, mapLike)

  implicit def diffForOptional[T](implicit ddt: Diff[T]): Diff[Option[T]] = new DiffForOption[T](ddt)

  implicit def diffForSet[C[_], T](implicit
      dt: Diff[T],
      matcher: ObjectMatcher[SetEntry[T]],
      setLike: SetLike[C]
  ): Diff[C[T]] = new DiffForSet[C, T](dt, matcher, setLike)

  implicit def diffForEither[L, R](implicit ld: Diff[L], rd: Diff[R]): Diff[Either[L, R]] =
    new DiffForEither[L, R](ld, rd)

  implicit def diffForSeq[C[_], T](implicit
      dt: Diff[T],
      matcher: ObjectMatcher[SeqEntry[T]],
      seqLike: SeqLike[C]
  ): Diff[C[T]] = new DiffForSeq[C, T](dt, matcher, seqLike)
}

trait LowPriorityDiff {

  /** Implicit instance of Diff[T] created from implicit Derived[Diff[T]]. Should not be called explicitly from clients
    * code. Use `summon` instead.
    * @param dd
    * @tparam T
    * @return
    */
  implicit def derivedDiff[T](implicit dd: Derived[Diff[T]]): Diff[T] = dd.value

  /** Returns unwrapped instance of Diff[T] from implicitly summoned Derived[Diff[T]]. Use this method when you want to
    * modify auto derived instance of diff and put it back into the implicit scope.
    * @param dd
    * @tparam T
    * @return
    */
  def summon[T](implicit dd: Derived[Diff[T]]): Diff[T] = dd.value
}

case class Derived[T](value: T) extends AnyVal

case class DiffLens[T, U](outer: Diff[T], path: List[ModifyPath]) {
  def setTo(d: Diff[U]): Diff[T] = using(_ => d)

  def using(mod: Diff[U] => Diff[U]): Diff[T] = {
    outer.modifyUnsafe(path: _*)(mod)
  }

  def ignore(implicit config: DiffConfiguration): Diff[T] = outer.modifyUnsafe(path: _*)(config.makeIgnored)
}

sealed trait ModifyPath extends Product with Serializable
object ModifyPath {
  case class Field(name: String) extends ModifyPath
  case object Each extends ModifyPath
  case object EachKey extends ModifyPath
  case object EachValue extends ModifyPath
  case class Subtype[T](owner: String, short: String) extends ModifyPath
}
