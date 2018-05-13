package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    def merge(s1: Seq[Int], s2: Seq[Int], acc: Seq[Int]): Seq[Int] = {
      (s1, s2) match {
        case (Nil, head2) => acc ++ head2
        case (head1, Nil) => acc ++ head1
        case (head1::tail1, head2::tail2) if (head2 > head1) => merge(tail1, s2, acc :+ head1)
        case (head1::tail1, head2::tail2) => merge(s1, tail2, acc :+ head2)
      }
    }

    data match {
      case value if (value.length == 1) => value
      case list => {
        val (left, right) = data.splitAt((list.size + 1) /2)
        merge(mergeSort(left), mergeSort(right), Nil)
      }
    }
  }
}

