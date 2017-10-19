package lectures.operators

import scala.util.Try

/**
  * Проходит чемпионат по спортивному киданю костей)
  * Сражаются "Наши" и "Приезжие"
  *
  * Каждый член команды бросил кубик и должен сравнить свой результат с каждым результатом из команды соперника
  *
  * Итог сравнений должн быть записан в ассоциативный массив в таком виде
  * val results: Array[(String, Int)] = (("Artem vs John" -> 3), ("Artem vs James" -> 5), ... )
  * При этом числовое значение должно быть получено как разность между результатами первого и второго игроков
  *
  * Когда составлен массив results, надо подсчитать, чья взяла.
  * Если результат встречи >0, то finalResult увеличивается на единицу
  * Если <0, уменьшается
  *
  * В итоге надо напечатать:
  * "Наша взяла", если наших побед больше, т.е. finalResult > 0
  * "Продули", если победили приезжие
  * "Победила дружба" в случае ничьи
  *
  * Для решения задачи раскомментируйте тело объекта Competition
  */

object Competition {

  val locals = Map("Artem" -> 6, "Sergey" -> 5, "Anton" -> 2, "Vladimir" -> "2", "Alexander" -> 4l)
  val foreigners = Map[String, Int]("John" -> 3, "James" -> 1, "Tom" -> 2, "Dick" -> 5, "Eric" -> 6)

  val results = for (l <- locals.toArray.map(pair => (pair._1, toInt(pair._2))); f <- foreigners) yield l._1 + " vs " + f._1 -> (l._2 - f._2)

  def toInt(x: Any) = x match {
    case value: Int => value
    case value: Long => value.toInt
    case value: String => value.toInt
  }

  var finalResult = 0
  for (r <- results) {
    if (r._2 > 0) finalResult = finalResult + 1
    else if (r._2 < 0) finalResult -= 1
  }

  finalResult match {
    case a if a > 0 => println("Наша взяла")
    case a if a < 0 => println("Продули")
    case _ => println("Победила дружба")
  }
}