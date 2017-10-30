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

  def add(newValue: Int): BST = {
    if (newValue < value) {
      if (left.isDefined) this.copy(left = Option(left.get.add(newValue).asInstanceOf[BSTImpl])) else this.copy(left = Option(BSTImpl(newValue)))
    }
    else {
      if (right.isDefined) this.copy(right = Option(right.get.add(newValue).asInstanceOf[BSTImpl])) else this.copy(right = Option(BSTImpl(newValue)))
    }
  }

  def find(value: Int): Option[BST] = {
    if (value == this.value) Option(this)
    else if (value > this.value) right
    else left
  }

  override def toString: String = {
    def depthOfBSTAndMaxValue(acc: Int, bst: BST): (Int, Int) = bst match {
      case BSTImpl(_, leftVal, rightVal) if leftVal.isDefined && rightVal.isDefined =>
        val (leftRes, rightRes) = (depthOfBSTAndMaxValue(acc + 1, leftVal.get), depthOfBSTAndMaxValue(acc + 1, rightVal.get))
        (math.max(leftRes._1, rightRes._1), math.max(leftRes._2, rightRes._2))
      case BSTImpl(_, leftVal, _) if leftVal.isDefined => depthOfBSTAndMaxValue(acc + 1, leftVal.get)
      case BSTImpl(_, _, rightVal) if rightVal.isDefined =>  depthOfBSTAndMaxValue(acc + 1, rightVal.get)
      case _ => (acc, bst.value)
    }
    val (depth, maxValue) = depthOfBSTAndMaxValue(1, this)

    val maxLength = maxValue.toString.toCharArray.length

    def fromValueToStr(value: Option[Int], maxLengthOfValue: Int): String = {
      if (value.isEmpty) " " * maxLengthOfValue
      else {
        val startedString = value.get.toString
        val diffOfMaxAndCurrentString = maxLengthOfValue - startedString.length
        if (diffOfMaxAndCurrentString % 2 == 0) " " * (diffOfMaxAndCurrentString / 2) + startedString + " " * (diffOfMaxAndCurrentString / 2)
        else " " + " " * (diffOfMaxAndCurrentString / 2) + startedString + " " * (diffOfMaxAndCurrentString / 2)
      }
    }

    def merge2Arrays(arr1: Array[Option[Int]], arr2: Array[Option[Int]]): Array[Option[Int]] = {
      val sizeOfRes = math.min(arr1.length, arr2.length)
      val arrRes = Array.ofDim[Option[Int]](sizeOfRes)
      (0 until sizeOfRes).foreach(index => arrRes(index) = index match {
        case ind if arr1(ind).isDefined => arr1(ind)
        case ind if arr2(ind).isDefined => arr2(ind)
        case _ => None
      })
      arrRes
    }

    val arrayOfTree: Array[Option[Int]] = Array[Option[Int]]((0 until math.pow(2, depth).toInt - 1).map(_ => None): _*)
    def bstToArray(root: BST, indexOfRoot: Int, currentArray: Array[Option[Int]]): Array[Option[Int]] = root match {
      case head if head.left.isDefined && head.right.isDefined => merge2Arrays(bstToArray(head.left.get, indexOfRoot * 2 + 1, currentArray), bstToArray(head.right.get, indexOfRoot * 2 + 2, currentArray)).patch(indexOfRoot, Array(Some(head.value)), 1)
      case head if head.left.isDefined => bstToArray(head.left.get, indexOfRoot * 2 + 1, currentArray).patch(indexOfRoot, Array(Some(head.value)), 1)
      case head if head.right.isDefined => bstToArray(head.right.get, indexOfRoot * 2 + 2, currentArray).patch(indexOfRoot, Array(Some(head.value)), 1)
      case head => currentArray.patch(indexOfRoot, Array(Some(head.value)), 1)
    }

    val graphAsArray = bstToArray(this, 0, arrayOfTree)

    def array2StringAsTree(array: Array[Option[Int]], depth: Int, maxLength: Int, symbolsBetweenNodes: Int): String = {
      val list = array.toList
      val width = math.pow(2, depth).toInt * maxLength + symbolsBetweenNodes

      def matchListOfElems(list: List[Option[Int]], currentString: String = "", numOfValuesAtCurrentLevel: Int = 1, currentPos: Int = 1, lenOfStringForValue: Int = width): String = list match {
        case Nil => currentString
        case head :: tail =>
          if (currentPos != numOfValuesAtCurrentLevel) {
            matchListOfElems(tail, currentString + fromValueToStr(head, lenOfStringForValue), numOfValuesAtCurrentLevel, currentPos + 1, lenOfStringForValue)
          }
          else {
            val numOfValuesAtNextLevel = numOfValuesAtCurrentLevel * 2
            val lenOfStringForNextLevel = width / numOfValuesAtNextLevel
            matchListOfElems(tail, currentString + fromValueToStr(head, lenOfStringForValue) + "\n", numOfValuesAtNextLevel, 1, lenOfStringForNextLevel)
          }
      }
      matchListOfElems(list)
    }

    array2StringAsTree(graphAsArray, depth, maxLength, 2)
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
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)

  println(testTree)
}