package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    def merge(s1: Seq[Int], s2: Seq[Int], acc: Seq[Int]): Seq[Int] = {
      (s1, s2) match {
        case (v1, v2) if (v1.length == 0) => acc ++ v2
        case (v1, v2) if (v2.length == 0) => acc ++ v1
        case (v1::v3, v2::v4) if (v2 > v1) => merge(v3, Seq[Int](v2) ++ v4, acc ++ Seq[Int](v1))
        case (v1::v3, v2::v4) => merge(Seq[Int](v1) ++ v3, v4, acc ++ Seq[Int](v2))
      }
    }

    data match {
      case value if (value.length == 1) => value
      case _ => {
        val buf = data.zipWithIndex.partition(_._2 < 5 / 2 - 1)
        merge(mergeSort(buf._1.unzip._1), mergeSort(buf._2.unzip._1), Nil)
      }
    }
  }

}

