package lectures.oop.types

import lectures.matching.SortingStuff.Watches

import scala.math.Ordering
import scala.util.Random

/**
  * Модифицируйте реализацию BSTImpl из предыдущего задания.
  * Используя тайп параметры и паттерн Type Class, реализуйте GeneralBSTImpl таким образом,
  * чтобы дерево могло работать с произвольным типом данных.
  *
  * Наследников GeneralBSTImpl определять нельзя.
  *
  * Создайте генератор для деревьев 3-х типов данных:
  * * * * float
  * * * * String
  * * * * Watches из задачи SortStuff. Большими считаются часы с большей стоимостью
  */

trait GeneralBST[T] {
  val value: T
  val left: Option[GeneralBST[T]]
  val right: Option[GeneralBST[T]]

  def add(newValue: T): GeneralBST[T]

  def find(value: T): Option[GeneralBST[T]]
}


case class GeneralBSTImpl[T](value: T,
                             left: Option[GeneralBSTImpl[T]] = None,
                             right: Option[GeneralBSTImpl[T]] = None)(implicit ord: Ordering[T]) extends GeneralBST[T] {

  override def add(newValue: T): GeneralBST[T] = addGeneralBSTImpl(newValue)

  private def addGeneralBSTImpl(newValue: T): GeneralBSTImpl[T] = {
    import Ordered._

    val a: Int = 1
    if (newValue < value) {
      left.fold(
        this.copy(
          left = Some(GeneralBSTImpl(newValue))
        )
      )( bst_left =>
        this.copy(
          left = Some(bst_left.addGeneralBSTImpl(newValue))
        )
      )
    }
    else {
      right.fold(
        this.copy(
          right = Some(GeneralBSTImpl(newValue))
        )
      )( bst_right =>
        this.copy(
          right = Some(bst_right.addGeneralBSTImpl(newValue))
        )
      )
    }
  }

  override def find(value: T): Option[GeneralBST[T]] = {
    if (ord.equiv(value, this.value)) Some(this)
    else if (ord.gt(this.value, value)) left.flatMap(_.find(value))
    else right.flatMap(_.find(value))
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

object FloatTreeTest extends App {
  private def genRandomFloat() = Random.nextFloat() * maxValue

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = genRandomFloat()
  val markerItem2 = genRandomFloat()
  val markerItem3 = genRandomFloat()

  // Generate huge tree
  def floatGeneratror(bstTree: GeneralBST[Float], acc: Int = 0): GeneralBST[Float] = acc match {
    case value if value == nodesCount - 1 => bstTree
    case _ => floatGeneratror(bstTree.add(genRandomFloat()), acc + 1)
  }

  val root = GeneralBSTImpl[Float](maxValue / 2)
  val tree = floatGeneratror(root)

  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).map(_.value).contains(markerItem))
  require(testTree.find(markerItem2).map(_.value).contains(markerItem2))
  require(testTree.find(markerItem3).map(_.value).contains(markerItem3))

  println(testTree)
}

object StringTreeTest extends App {
  def genRandomString() = Random.nextString(maxStringSize)

  val sc = new java.util.Scanner(System.in)
  val maxStringSize = 6
  val nodesCount = sc.nextInt()

  val markerItem = genRandomString()
  val markerItem2 = genRandomString()
  val markerItem3 = genRandomString()

  // Generate huge tree
  def floatGeneratror(bstTree: GeneralBST[String], acc: Int = 0): GeneralBST[String] = acc match {
    case value if value == nodesCount - 1 => bstTree
    case _ => floatGeneratror(bstTree.add(genRandomString()), acc + 1)
  }

  val root = GeneralBSTImpl[String](genRandomString())
  val tree = floatGeneratror(root)

  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).map(_.value).contains(markerItem))
  require(testTree.find(markerItem2).map(_.value).contains(markerItem2))
  require(testTree.find(markerItem3).map(_.value).contains(markerItem3))

  println(testTree)
}

object WatchesTreeTest extends App {

  implicit val ord = new Ordering[Watches] {
    override def compare(x: Watches, y: Watches): Int = x.cost compare y.cost
  }

  private val brands = Seq(
    "Nautica",
    "Breitling",
    "Electronika",
    "Zarya",
    "Casio",
    "Citizen"
  )

  private def genRandomWhatches() = {
    val randomCost = (Math.random() * maxValue).toFloat
    val randomBrand = brands(Random.nextInt(brands.size))
    Watches(randomBrand, randomCost)
  }

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = genRandomWhatches()
  val markerItem2 = genRandomWhatches()
  val markerItem3 = genRandomWhatches()

  // Generate huge tree
  def floatGeneratror(bstTree: GeneralBST[Watches], acc: Int = 0): GeneralBST[Watches] = acc match {
    case value if value == nodesCount - 1 => bstTree
    case _ => floatGeneratror(bstTree.add(genRandomWhatches()), acc + 1)
  }

  val root = GeneralBSTImpl[Watches](genRandomWhatches().copy(cost = maxValue / 2))
  val tree = floatGeneratror(root)

  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).map(_.value).contains(markerItem))
  require(testTree.find(markerItem2).map(_.value).contains(markerItem2))
  require(testTree.find(markerItem3).map(_.value).contains(markerItem3))

  println(testTree)
}