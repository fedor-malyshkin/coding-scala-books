package frdomain.ch03.algebra

import java.util.Date

import scala.util.Try

import org.scalatest.funsuite.AnyFunSuite

class Algebra03Test extends AnyFunSuite {

  test("[3.1] test Try") {
    trait AccountService[Account, Amount, Balance] {
      def open(no: String, name: String, openingDate: Option[Date]): Try[Account]

      def close(account: Account, closeDate: Option[Date]): Try[Account]

      def debit(account: Account, amount: Amount): Try[Account]

      def credit(account: Account, amount: Amount): Try[Account]

      def balance(account: Account): Try[Balance]

      def transfer(from: Account, to: Account, amount: Amount): Try[(Account, Account, Amount)] =
        for {
          a <- debit(from, amount)
          b <- credit(to, amount)
        } yield (a, b, amount)
    }
  }

  test("[3.1] test Either") {
    type Error = String
    trait AccountService[Account, Amount, Balance] {
      def open(no: String, name: String, openingDate: Option[Date]): Either[Error, Account]

      def close(account: Account, closeDate: Option[Date]): Either[Error, Account]

      def debit(account: Account, amount: Amount): Either[Error, Account]

      def credit(account: Account, amount: Amount): Either[Error, Account]

      def balance(account: Account): Either[Error, Balance]

      def transfer(
        from: Account,
        to: Account,
        amount: Amount
      ): Either[Error, (Account, Account, Amount)] =
        for {
          a <- debit(from, amount)
          b <- credit(to, amount)
        } yield (a, b, amount)
    }
  }

  test("[3.1] test Option") {
    trait AccountService[Account, Amount, Balance] {
      def open(no: String, name: String, openingDate: Option[Date]): Option[Account]

      def close(account: Account, closeDate: Option[Date]): Option[Account]

      def debit(account: Account, amount: Amount): Option[Account]

      def credit(account: Account, amount: Amount): Option[Account]

      def balance(account: Account): Option[Balance]

      def transfer(from: Account, to: Account, amount: Amount): Option[(Account, Account, Amount)] =
        for {
          a <- debit(from, amount)
          b <- credit(to, amount)
        } yield (a, b, amount)
    }
  }

}
