package lectures.oop

import scala.util.Random


/**
  * BSTImpl - это бинарное дерево поиска, содержащее только значения типа Int
  *
  * * Оно обладает следующими свойствами:
  * * * * * левое поддерево содержит значения, меньшие значения родителя
  * * * * * правое поддерево содержит значения, большие значения родителя
  * * * * * значения, уже присутствующие в дереве, в него не добавляются
  * * * * * пустые значения (null) не допускаются
  *
  * * Завершите реализацию методов кейс класс BSTImpl:
  * * * * * Трейт BST и BSTImpl разрешается расширять любым образом
  * * * * * Изменять сигнатуры классов и методов, данные в условии, нельзя
  * * * * * Постарайтесь не использовать var и мутабильные коллекции
  * * * * * В задаче про распечатку дерева, нужно раскомментировать и реализовать метод toString()
  *
  * * Для этой структуры нужно реализовать генератор узлов.
  * * Генератор:
  * * * * * должен создавать дерево, содержащее nodesCount узлов.
  * * * * * не должен использовать переменные или мутабильные структуры.
  *
  */
trait BST {
  val value: Int
  val left: Option[BST]
  val right: Option[BST]

  def add(newValue: Int): BST

  def find(value: Int): Option[BST]
}

case class BSTImpl(value: Int,
                   left: Option[BSTImpl] = None,
                   right: Option[BSTImpl] = None) extends BST {

  override def add(newValue: Int): BST = {
    if (newValue < value) {
      left.fold(
        this.copy(
          left = Some(BSTImpl(newValue))
        )
      )( bst_left =>
        this.copy(
          left = Some(bst_left.add(newValue).asInstanceOf[BSTImpl])
        )
      )
    }
    else {
      right.fold(
        this.copy(
          right = Some(BSTImpl(newValue))
        )
      )( bst_right =>
        this.copy(
          right = Some(bst_right.add(newValue).asInstanceOf[BSTImpl])
        )
      )
    }
  }

  override def find(value: Int): Option[BST] = {
    if (value == this.value) Some(this)
    else if (value > this.value) right.flatMap(_.find(value))
    else left.flatMap(_.find(value))
  }

  override def toString: String = {
    val leftToString = left.fold("")(_.toString)
    val rightToString = right.fold("")(_.toString)
    val currentValueToString = value.toString

    val newChildesHeight = getNewHeightOfChildes(leftToString, rightToString)

    val newWidth = getNewWidth(leftToString, rightToString, currentValueToString)

    mergeLeftRightTop(leftToString, rightToString, currentValueToString, newChildesHeight, newWidth)
  }

  //private

  private val lengthOfSeparator: Int = 2

  private def getNewWidth(leftStr: String, rightStr: String, topStr: String): Int = {
    val maxWidthOfChildes = Math.max(rightStr.getWidth, leftStr.getWidth)

    val newWidth = Math.max(2 * maxWidthOfChildes + lengthOfSeparator, topStr.getWidth)
    if (newWidth % 2 == 0) newWidth
    else newWidth + 1
  }

  private def getNewHeightOfChildes(leftStr: String, rightStr: String): Int = Math.max(leftStr.getHeight, rightStr.getHeight)

  private def mergeLeftRightTop(leftStr: String, rightStr: String, topStr: String, newChildesHeight: Int, width: Int) = {
    val childWidth = (width - lengthOfSeparator) / 2
    val normalizedLeft = leftStr.normalizeHeightAndWidth(newChildesHeight, childWidth)
    val normalizedRight = rightStr.normalizeHeightAndWidth(newChildesHeight, childWidth)
    val normalizedTopStr = topStr.normalizeStringSize(width)
    val mergedLeftAndRightStrs =
      normalizedLeft.split("\n").zip(normalizedRight.split("\n"))
        .map(pair => pair._1 + " " * lengthOfSeparator + pair._2).mkString("\n")
    normalizedTopStr + "\n" + mergedLeftAndRightStrs
  }

  private implicit class MyRichString(str: String) {
    def normalizeStringSize(maxLengthOfValue: Int): String = {
      val diffOfMaxAndCurrentString = maxLengthOfValue - str.length
      if (diffOfMaxAndCurrentString % 2 == 0) " " * (diffOfMaxAndCurrentString / 2) + str + " " * (diffOfMaxAndCurrentString / 2)
      else " " + " " * (diffOfMaxAndCurrentString / 2) + str + " " * (diffOfMaxAndCurrentString / 2)
    }

    def getHeight: Int = str.split('\n').length

    def getWidth: Int = {
      val index = str.indexOf('\n')
      if (index == -1) str.length
      else index
    }

    def normalizeWidth(width: Int): String = str.split("\n").map(_.normalizeStringSize(width)).mkString("\n")

    def normalizeHeight(height: Int): String = str + Seq.fill[String](height - str.getHeight)("\n" + " " * str.getWidth).mkString("")

    def normalizeHeightAndWidth(height: Int, width: Int): String = {
      str.normalizeWidth(width).normalizeHeight(height)
    }
  }
}

object TreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toInt
  val markerItem2 = (Math.random() * maxValue).toInt
  val markerItem3 = (Math.random() * maxValue).toInt

  // Generate huge tree
  def generatror(bstTree: BST, acc: Int = 0): BST = acc match {
    case value if value == nodesCount - 1 => bstTree
    case _ => generatror(bstTree.add(Random.nextInt(maxValue)), acc + 1)
  }

  val root: BST = BSTImpl(maxValue / 2)
  val tree: BST = generatror(root)

  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).map(_.value).contains(markerItem))
  require(testTree.find(markerItem2).map(_.value).contains(markerItem2))
  require(testTree.find(markerItem3).map(_.value).contains(markerItem3))

  println(testTree)
}