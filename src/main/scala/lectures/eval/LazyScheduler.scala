import java.time.{Clock, Instant}

import scala.collection.SeqView

/**
  * В этом задании ваша задча реализовать своеобразный View с таймером.
  *
  * Он должен представлять из себя стандартный SeqView c ограничением по времени.
  * Т.е. этот view ведет себя как обычно, пока не истечет таймаут, предеданный при создании.
  * Как только таймаут истекает, view должен начать вести себя так, как будто коллекция пуста.
  *
  * Для решения задачи подставьте на место вопросительных знаков реализацию view.
  * Раскомментируйте и выполните тесты в lectures.eval.LazySchedulerTest
  */

object LazySchedulerView {

  implicit class SeqViewConverter[A](f: Seq[A]) {
    val c: Clock = Clock.systemDefaultZone()

    class SeqViewWithTimer(timeUntilExists: Instant, seq: Seq[A]) extends SeqView[A, Seq[_]] {
      def isEmptySeq: Boolean = c.instant().isAfter(timeUntilExists)

      override protected def underlying: Seq[_] = if (!isEmptySeq) seq else Seq.empty[A]

      override def length: Int = if (!isEmptySeq) seq.length else 0

      override def apply(idx: Int): A = if (!isEmptySeq) seq(idx) else Seq.empty[A](idx)

      override def iterator: Iterator[A] = if (!isEmptySeq) seq.toIterator else Seq.empty[A].iterator
    }
    /**
      *
      * @param expirationTimeout - таймаут, после которого view становится пустым, в миллисекундах
      * @return - view
      */
    def lazySchedule(expirationTimeout: Long): SeqView[A, Seq[_]]  = {
      val i = c.instant().plusMillis(expirationTimeout)
      new SeqViewWithTimer(i, f)
    }
  }
}

object LazySchedulerViewExample extends App {

  import LazySchedulerView._

  val v = List(1, 2, 3, 56)
  val d = v.lazySchedule(1300)

  print(d.length)
  Thread.sleep(1500)
  print(d.length)
}


