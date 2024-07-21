package ru.otus.module1.DataCollection1.homework

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Random



// В урне 3 белых и 3 черных шара. Из урны дважды вынимают по одному шару, не возвращая их обратно.
// Найти вероятность появления белого шара

// Расчеты:

// Появление белого шара - А
// Появление черного шара = В
//
// Вероятность появления белого шара после двух вытаскиваний из корзин:
//  P(AB) + P(BA) + P(AA) = 1 - P(BB) =
//  1 - P(B) * P(B|B)
//  = 1 - 1/2 * 2/5
//  = 1 - 2/10
//  = 8/10

class BallsExperiment {

  private val basket = List(1, 1, 1, 0, 0, 0)

  @tailrec
  private def takeFromBasket(times: Int,
                             basketCondition: List[Int] = basket,
                             acc: List[Int] = List.empty): List[Int] = {
    times match {
      case 0 => acc
      case t if t > 0 =>
        basketCondition match {
          case t :: _ =>
            val hero = Random.between(0, basketCondition.length)
            takeFromBasket(
              times - 1,
              basketCondition.foldLeft((ListBuffer.empty[Int], 0)) { case (iacc, bc) =>
                if (iacc._2 != hero) (iacc._1 :+ bc, iacc._2 + 1)
                else (iacc._1, iacc._2 + 1)
              }._1.toList,
              basketCondition(hero) :: acc
            )
          case _ => acc
        }
      case t if t < 0 => List.empty
    }
  }

  def isWhiteAppeared: Boolean = {
    takeFromBasket(2).contains(1)
  }
}

object BallsTest {
  def main(args: Array[String]): Unit = {
    val count = 10000
    val listOfExperiments: List[BallsExperiment] = (1 to count).map(_ => new BallsExperiment()).toList
    val expResults: List[Boolean] = listOfExperiments.map(_.isWhiteAppeared)
    val countOfPositiveExperiments: Float = expResults.count(_ == true)
    println(countOfPositiveExperiments / count)
  }
}