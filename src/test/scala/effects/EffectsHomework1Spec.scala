package effects

import effects.EffectsHomework1._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime

import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor}

class EffectsHomework1Spec extends AnyFreeSpec with Matchers {
  "unit" - {
    IO.unit.unsafeRunSync() should be(())
  }

  "unsafeRunSync should throw unhandled exception" in {
    a [ArithmeticException] should be thrownBy IO(1 / 0).unsafeRunSync()
  }

  "map" - {
    "should work" in {
      IO(1).map(_ + 2).unsafeRunSync() should be(3)
    }

    "should be lazy" in {
      val col = mutable.ArrayBuffer("I'm just exist")
      val io = IO.unit.map(_ => col.clear())
      col.length should be(1)
      io.unsafeRunSync()
      col.length should be(0)
    }
  }

  "flatMap" - {
    "should work" in {
      IO(1).flatMap(x => IO(x + 2)).unsafeRunSync() should be(3)
    }

    "should be lazy" in {
      val col = mutable.ArrayBuffer("I'm just exist")
      val io = IO.unit.flatMap(_ => IO(col.clear()))
      col.length should be(1)
      io.unsafeRunSync()
      col.length should be(0)
    }
  }

  "*> should eval and replace" - {
    val col = mutable.ArrayBuffer[String]()
    val io = IO(col.addOne("just arrived and replaced")) *> IO(2)
    col should be (empty)
    io.unsafeRunSync() should be (2)
    col should contain theSameElementsAs List("just arrived and replaced")
  }

  "as should replace value after eval" - {
    val col = mutable.ArrayBuffer[String]()
    val io = IO(col.addOne("just arrived and replaced")).as(2)
    col should be (empty)
    io.unsafeRunSync() should be (2)
    col should contain theSameElementsAs List("just arrived and replaced")
  }

  "void should eval IO and discard result" - {
    val col = mutable.ArrayBuffer[String]()
    val io = IO(col.addOne("just arrived and discarded")).void
    col should be (empty)
    io.unsafeRunSync() should be (())
    col should contain theSameElementsAs List("just arrived and discarded")
  }

  "attempt" - {
    "right" in {
      IO(1 + 2).attempt.unsafeRunSync() should be (Right(3))
    }

    "left" in {
      IO(1 / 0).attempt.unsafeRunSync() should matchPattern { case Left(ex: ArithmeticException) => }
    }
  }

  "option" - {
    "none for exception" in {
      IO(1 / 0).option.unsafeRunSync() should be (None)
    }

    "some if calculated successfully" in {
      IO(1 + 2).option.unsafeRunSync() should be (Some(3))
    }
  }

  "handleErrorWith" - {
    "should recover" in {
      IO(1 / 0).handleErrorWith(t => IO(0)).unsafeRunSync() should be (0)
    }

    "should return result" in {
      IO(1 + 2).handleErrorWith(t => IO(0)).unsafeRunSync() should be (3)
    }

    "should chain, not eval" in {
      val col = mutable.ArrayBuffer[String]()
      val io = IO(col.addOne("just arrived")).handleErrorWith(t => IO(col))
      col should be (empty)
      io.unsafeRunSync()
      col should contain theSameElementsAs List("just arrived")
    }
  }

  "redeem" - {
    "should map" in {
      IO(1 + 2).redeem(t => 0, _ + 1).unsafeRunSync() should be (4)
    }

    "should recover" in {
      IO(1 / 0).redeem(t => 0, _ => 2).unsafeRunSync() should be (0)
    }

    "should chain, not eval" in {
      val col = mutable.ArrayBuffer[String]()
      val io = IO(col.addOne("just arrived")).redeem(t => 0, _ => 1)
      col should be (empty)
      io.unsafeRunSync()
      col should contain theSameElementsAs List("just arrived")
    }
  }

  "redeemWith" - {
    "should flatMap" in {
      IO(1 + 2).redeemWith(t => IO(0), x => IO(x + 1)).unsafeRunSync() should be (4)
    }

    "should recover" in {
      IO(1 / 0).redeemWith(t => IO(0), x => IO(x + 1)).unsafeRunSync() should be (0)
    }

    "should chain, not eval" in {
      val col = mutable.ArrayBuffer[String]()
      val io = IO(col.addOne("just arrived")).redeemWith(t => IO(0), _ => IO(1))
      col should be (empty)
      io.unsafeRunSync()
      col should contain theSameElementsAs List("just arrived")
    }
  }

  "unsafeRunSync" - {
    "should run enclosed function and return" in {
      IO(1 + 2).unsafeRunSync() should be (3)
    }

    "should throw exception if it occurs" in {
      a [ArithmeticException] should be thrownBy IO(1 / 0).unsafeRunSync()
    }
  }

  "unsafeToFuture" - {
    implicit val ec: ExecutionContextExecutor = ExecutionContext.global

    "should pass calculation to Future" in {
      Await.result(IO(1 + 2).unsafeToFuture, 1.second) should be (3)
    }

    "should start execution inside Future eagerly" in {
      val col = mutable.ArrayBuffer[String]()
      val io = IO(col.addOne("just arrived"))
      col should be (empty)
      Await.result(io.unsafeToFuture, 1.second)
      col should contain theSameElementsAs List("just arrived")
    }
  }

  "suspend" - {
    "should not evaluate thunk" in {
      lazy val thunk = {
        throw new RuntimeException("thunk has been evaluated when it shouldn't!")
        IO(1)
      }
      IO.suspend(thunk)
    }
  }

  "raiseError" - {
    "should result in exception if run unsafely" in {
      val io = IO.raiseError(new RuntimeException("raised for test"))
      a [RuntimeException] should be thrownBy io.unsafeRunSync()
    }

    "should exit further evaluation" in {
      val io = IO.raiseError[Int](new RuntimeException).map(_ + 1)
      a [RuntimeException] should be thrownBy io.unsafeRunSync()
    }
  }

  "raiseWhen/Unless" - {
    "raiseUnless should raise exception if cond is false" in {
      val io = IO.raiseUnless(cond = false)(new RuntimeException)
      a[RuntimeException] should be thrownBy io.unsafeRunSync()
    }

    "raiseWhen should raise exception if cond is true" in {
      val io = IO.raiseWhen(cond = true)(new RuntimeException)
      a[RuntimeException] should be thrownBy io.unsafeRunSync()
    }
  }

  "whenA/unlessA" - {
    "unlessA should evaluate action if cond is false" in {
      val io = IO()
      IO.unlessA(cond = false)(io) should be theSameInstanceAs io
    }

    "whenA should return action if cond is true" in {
      val io = IO()
      IO.whenA(cond = true)(io) should be theSameInstanceAs io
    }
  }

}
