package io.getquill.monad

import com.twitter.util.Future
import io.getquill.context.Context
import com.twitter.util.Return
import scala.util.Success
import com.twitter.util.Throw
import scala.util.Failure
import com.twitter.util.Try

trait TwitterFutureIOMonad extends IOMonad {
  this: Context[_, _] =>

  type Result[T] = Future[T]

  def unsafePerformIO[T](io: IO[T, _]): Result[T] =
    io match {
      case FromTry(t) => Future.const(Try(t.get))
      case Run(f)     => f()
      case Sequence(in, cbf) =>
        Future.collect(in.map(unsafePerformIO).toSeq)
          .map(cbf(_).result)
      case TransformWith(a, fA) =>
        unsafePerformIO(a)
          .liftToTry.map {
            case Return(v) => Success(v)
            case Throw(t)  => Failure(t)
          }
          .flatMap(v => unsafePerformIO(fA(v)))
    }
}
