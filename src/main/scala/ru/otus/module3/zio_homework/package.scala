package ru.otus.module3

import ru.otus.module3.zioConcurrency.{currentTime, printEffectRunningTime}
import ru.otus.module3.zio_homework.PrintRunningTime.Service
import ru.otus.module3.zio_homework.config.{AppConfig, load}

import scala.language.postfixOps
import zio._
import zio.clock.Clock
import zio.console._
import zio.random._

import java.time.Duration
import javax.naming.ConfigurationException
import scala.io.{BufferedSource, Source}


package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */



  lazy val guessProgram = {

    val askHello = putStrLn("Hello! Try to guess a number from 1 to 3:")
    val getUserAnswer = getStrLn.map(_.toInt)
    val getSystemAnswer = nextInt.map(_.abs % 3 + 1)

    def chooseOutput(answer: Int,
                     trueAnswer: Int): String =
      if (answer != trueAnswer)
        s"Oops! You're wrong! Right answer is $trueAnswer but your answer is $answer."
      else s"Woah, you're awesome! Your answer is right: $trueAnswer! "


    for {
      _ <- askHello
      systemAnswer <- getSystemAnswer
      userAnswer <- getUserAnswer
      _ <- putStrLn(chooseOutput(userAnswer, systemAnswer))
    } yield ExitCode.success


  }

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile(f : => Boolean): Task[ExitCode] = ZIO.effect(f)
    .flatMap(res => if (res) ZIO.succeed(ExitCode.success) else doWhile(f))

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */



  def loadConfigOrDefault: ZIO[Console, Exception, AppConfig] = {

    def openFile(filename: String): Task[BufferedSource] = ZIO.effect(Source.fromFile(filename))
    def closeFile(source: BufferedSource): Task[Unit] = ZIO.effect(source.close())


    val filePath = System.getProperty("user.dir") + "/src/main/scala/ru/otus/module3/zio_homework/config.txt"
    ZManaged.make(openFile(filePath))(b => closeFile(b).orDie).use {
      bs =>

        val getDataFromFile: Task[Map[String, String]] =
          ZIO.effect(bs.getLines().foldLeft(Map.empty[String, String]) { (configMap, fileLine) =>
            val propName2propVal = fileLine.split(":").toList match {
              case name :: value :: Nil => name -> value
              case _ => throw new ConfigurationException("wrong config file")
            }
            configMap + propName2propVal
          })

        def map2config(config: Map[String, String]): Task[AppConfig] =
          ZIO.effect(config.get("host").zip(config.get("port")).fold(throw new ConfigurationException("wrong config file")) {
            config =>
              AppConfig(config._1, config._2)
          })

        for {
          _ <- putStrLn("Start uploading file")
          dataFromFile <- getDataFromFile
          _ <- putStrLn("File was read")
          config <- map2config(dataFromFile)
        } yield config
    }.catchAll {_ =>
      for {
        defaultConfig <- load
        _ <- putStrLn(s"There is an unknown problem with manual config file. Default configuration settings were uploaded: [host: ${defaultConfig.host}, port: ${defaultConfig.port}]")
      } yield defaultConfig
    }

  }

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: ZIO[Clock with Random, Nothing, Int] = nextInt.map(_.abs % 11).zipLeft(clock.sleep(Duration.ofSeconds(1)))

  /**
   * 4.2 Создайте коллекцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: Seq[ZIO[Clock with Random, Nothing, Int]] = (1 to 10).map(_ => eff).toVector

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app: ZIO[Clock with Random with Console, Nothing, Int] =
    printEffectRunningTime(ZIO.mergeAll(effects)(0)(_ + _))


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */


    // Вопрос: насколько оправданно запускать параллельные операции для совсем легковесных функций?
    // Знаю, что с фьючами это неоправданно (если запускать обработку на каждом элементе, не батчами),
    // А что насчет fiber?
  lazy val appSpeedUp: ZIO[Clock with Random with Console, Nothing, Int] = printEffectRunningTime(ZIO.mergeAllPar(effects)(0)(_ + _))

  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.console.putStrLn например
   */
  type PrintRunningTime = Has[Service]

  object PrintRunningTime {

    trait Service {
      def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock with Console, E, A]
    }

    val live: ULayer[PrintRunningTime] = ZLayer.succeed {

      new Service {
        override def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock with Console, E, A] = for{
          start <- currentTime
          r <- zio
          end <- currentTime
          _ <- putStrLn(s"Running time ${end - start}").orDie
        } yield r
      }
    }

    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with PrintRunningTime with Clock with Console, E, A] =
      ZIO.accessM(_.get.printEffectRunningTime(zio))

  }

   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg = PrintRunningTime.printEffectRunningTime(app)

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp: ZIO[Clock with Console with Random, Nothing, Int] =
    appWithTimeLogg.provideSomeLayer[Clock with Console with Random](PrintRunningTime.live)

}
