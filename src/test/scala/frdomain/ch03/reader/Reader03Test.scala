package frdomain.ch03.reader

import java.util.Date

import scala.collection.mutable
import scala.util.{Success, Try}

import cats.data.Reader
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Reader03Test extends AnyFunSuite {

  test("[3.3] test Registry: Reader composition") {

    type Account = String
    type Amount = BigDecimal

    trait AccountRepository[AccountT, BalanceT] {
      def balance(accountNo: String): Try[BalanceT]
      def updateBalance(accountNo: String, balance: BalanceT): Try[BalanceT]
      def openedOn(date: Date): Try[Seq[AccountT]]
    }

    trait AccountService[AccountT, AmountT] {
      type Repo = AccountRepository[AccountT, AmountT]
      def open(
        no: String,
        name: String,
        openingDate: Option[Date]
      ): Reader[Repo, Try[AccountT]]
      def close(no: String, closeDate: Option[Date]): Reader[Repo, Try[AccountT]]
      def debit(no: String, amount: AmountT): Reader[Repo, Try[AccountT]]
      def credit(no: String, amount: AmountT): Reader[Repo, Try[AccountT]]
      def balance(no: String): Reader[Repo, Try[AmountT]]
    }

    object AccountServiceImpl extends AccountService[Account, Amount] {
      override type Repo = AccountRepository[Account, Amount]
      override def open(
        no: String,
        name: String,
        openingDate: Option[Date]
      ): Reader[Repo, Try[Account]] = ???

      override def close(
        no: String,
        closeDate: Option[Date]
      ): Reader[Repo, Try[Account]] = ???

      override def debit(no: String, amount: Amount): Reader[Repo, Try[Account]] = Reader { a =>
        for {
          accInitial <- a.balance(no)
          debitBalance = accInitial + amount
          _ <- a.updateBalance(no, debitBalance)
        } yield no
      }

      override def credit(no: String, amount: Amount): Reader[Repo, Try[Account]] = Reader { a =>
        for {
          accInitial <- a.balance(no)
          creditBalance = accInitial - amount
          _ <- a.updateBalance(no, creditBalance)
        } yield no
      }

      override def balance(no: String): Reader[Repo, Try[Amount]] = Reader { a =>
        for {
          accInitial <- a.balance(no)
        } yield accInitial
      }
    }

    object App {
      import AccountServiceImpl._
      def op(accountNumber: String) = for {
        _ <- credit(accountNumber, BigDecimal(100))
        _ <- credit(accountNumber, BigDecimal(300))
        _ <- debit(accountNumber, BigDecimal(160))
        b <- balance(accountNumber)
      } yield b
    }

    val t: AccountRepository[Account, Amount] = new AccountRepository[Account, Amount] {
      val balances = mutable.AnyRefMap("123X" -> BigDecimal(1000))

      override def balance(accountNo: String): Try[Amount] =
        Success(balances.getOrElse(accountNo, BigDecimal(0)))

      override def updateBalance(accountNo: String, balance: Amount): Try[Amount] =
        Success(balances.put(accountNo, balance).getOrElse(BigDecimal.valueOf(0)))

      override def openedOn(date: Date): Try[Seq[Account]] = ???
    }
    App.op("123X").run(t)
    // 1000 - 100 - 300 + 160 = 760
    t.balance("123X") should be(Success(BigDecimal.valueOf(760)))
  }
}
