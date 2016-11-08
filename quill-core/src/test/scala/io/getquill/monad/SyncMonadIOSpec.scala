package io.getquill.monad

import io.getquill.MirrorContext

class SyncMonadIOSpec extends IOMonadSpec {

  override val ctx = new MirrorContext
  import ctx._

  override def eval[T](io: IO[T, _]) =
    unsafePerformIO[T](io)

}