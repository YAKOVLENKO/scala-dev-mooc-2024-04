package ru.otus.module1

import scala.annotation.tailrec
import scala.language.postfixOps



/**
 * referential transparency
 */




 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }


  def factRec(n: Int): Int =
    if(n <= 0) 1 else n * factRec(n - 1)

  def factTailRec(n: Int): Int = {

    def go(n: Int, accum: Int): Int =
      if(n <= 0) accum else go(n - 1, n * accum)
    go(n, 1)
  }

  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   */


}



object hof{

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result: B = f(a)
    val end = System.currentTimeMillis()
    println(s"Running time: ${end - start}")
    result
  }

  def doomy(string: String): Unit = {
    Thread.sleep(1000)
    println(string)
  }

  // изменение поведения ф-ции

  def not[A](f: A => Boolean): A => Boolean = a => !f(a)

  def isOdd(i: Int): Boolean = i % 2 > 0

  val isEven: Int => Boolean = not(isOdd)



  // изменение самой функции

  def partial[A, B, C](a: A,  f: (A, B) => C): B => C = b => f(a, b)

  def partial2[A, B, C](a: A,  f: (A, B) => C): B => C =
    f.curried(a)

  def sum(x: Int, y: Int): Int = x + y

  val p: Int => Int = partial(3, sum)
  p(2) // 5
  p(3) // 6
  partial(3, sum)(3) // 6



















}






/**
 *  Реализуем тип Option
 */



 object opt {


  class Animal
  class Dog extends Animal

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутсвие результата
   */
   // + covariance
   // - contravariance

  sealed trait Option[+T]{
    def isEmpty: Boolean = this match {
      case None => true
      case Some(v) => false
    }

    def get: T =  this match {
      case None => throw new Exception("get ob empty option")
      case Some(v) => v
    }

    def map[B](f: T => B): Option[B] = flatMap(t => Option(f(t)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Some(v) => f(v)
      case None => None
    }


    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny(): Unit = {
      this match {
        case Some(v) => println(v)
        case _ =>
      }
    }

    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[B](v: Option[B]): Option[(T, B)] = {
      (this, v) match {
        case (Some(t), Some(b)) => Some((t, b))
        case _ => None

      }
    }


    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */
    def filter(f: T => Boolean): Option[T] = {
      this.map(f) match {
        case Some(true) => this
        case _ => None
      }
    }



  }

  val opt1: Option[Int] = ???
  val opt2: Option[Int] = opt1.map(i => i + 1)

  case class Some[T](v: T) extends Option[T]
  case object None extends Option[Nothing]


  object Option{
    def apply[T](v: T): Option[T] =
      if(v == null) Some(v) else None
  }

 }

 object list {
   /**
    *
    * Реализовать односвязанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
    */

    sealed trait List[+T] {


     /**
      * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
      *
      */
       def ::[TT >: T](el: TT): List[TT] = new ::(el, this)

     /**
      * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
      *
      */
     def mkString(sep: Char): String = {

       @tailrec
       def strMaker(lst: List[String]): List[String] = {
         lst match {
           case Nil | _ :: Nil => lst
           case h :: p :: t =>
             strMaker(s"$h$sep$p" :: t)
         }
       }

       strMaker(this.map(_.toString)) match {
         case Nil => ""
         case t :: _ => t
       }

     }


     /**
      *
      * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
      */
     def reverse: List[T] = {

       @tailrec
       def tReverser(lst: List[T], acc: List[T] = Nil): List[T] = {
         lst match {
           case Nil => acc
           case h :: t => tReverser(t, h :: acc)
         }}

       tReverser(this)
     }


     /**
      *
      * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
      */
     def map[B](f: T => B): List[B] = {

       @tailrec
       def tMapper(lst: List[T], acc: List[B] = Nil): List[B] = {
         lst match {
           case Nil => acc
           case h :: t => tMapper(t, f(h) :: acc)
         }}

       tMapper(this).reverse

     }


     /**
      *
      * Реализовать метод filter для списка который будет фильтровать список по некому условию
      */
     def filter(f: T => Boolean): List[T] = {

       @tailrec
       def tFilter(lst: List[T], acc: List[T] = Nil): List[T] = {
         lst match {
           case Nil => acc
           case h :: t if f(h) => tFilter(t, h :: acc)
           case _ :: t => tFilter(t, acc)
         }
       }

       tFilter(this).reverse

     }


   }

   case class ::[T](head: T, tail: List[T]) extends List[T]
   case object Nil extends List[Nothing]

   object List{
     def apply[A](v: A*): List[A] = if(v.isEmpty) Nil
     else ::(v.head, apply(v.tail:_*))


     /**
      *
      * Написать функцию incList котрая будет принимать список Int и возвращать список,
      * где каждый элемент будет увеличен на 1
      */

     def incList(list: List[Int]): List[Int] = list.map(_ + 1)


     /**
      *
      * Написать функцию shoutString котрая будет принимать список String и возвращать список,
      * где к каждому элементу будет добавлен префикс в виде '!'
      */
     def shoutString(list: List[String]): List[String] = list.map(v =>s"$v!")


   }

   1 :: 2 :: 3 :: Nil

    /**
      * Конструктор, позволяющий создать список из N - го числа аргументов
      * Для этого можно воспользоваться *
      * 
      * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
      * def printArgs(args: Int*) = args.foreach(println(_))
      */

 }