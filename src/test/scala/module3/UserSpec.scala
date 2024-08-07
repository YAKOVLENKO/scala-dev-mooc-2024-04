package module3

import module3.emailService.EmailServiceMock
import module3.userDAO.UserDAOMock
import ru.otus.module3.emailService.{Email, EmailAddress, Html}
import ru.otus.module3.userService.{User, UserID, UserService}
import zio.console.Console
import zio.{Has, ZIO, ZLayer}
import zio.test.Assertion.{anything, equalTo, isUnit}
import zio.test.environment.TestConsole
import zio.test.mock.Expectation.{unit, value}
import zio.test.{DefaultRunnableSpec, ZSpec, ZTestEnv, assertM, suite, testM}
import zio.test._
import zio.test.mock.Expectation
import zio.test.mock.Result

object UserSpec extends DefaultRunnableSpec{
  override def spec = suite("User spec"){
    testM("notify user"){

      val sendMailMock = EmailServiceMock.SendMail(
        equalTo(Email(EmailAddress("test@test.com"), Html("Hello here"))), unit)

      val daoMock = UserDAOMock.FindBy(
        equalTo(UserID(1)),
        value(Some(User(UserID(1), EmailAddress("test@test.com"))))
      )

      val layer = daoMock >>> UserService.live ++ sendMailMock

      assertM(UserService.notifyUser(UserID(1)).provideSomeLayer[TestConsole with Console](layer))(anything)
    }
  }
}
