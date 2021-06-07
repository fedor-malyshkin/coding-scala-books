package frdomain.ch04.patterns

import java.util.Date

import cats.Semigroupal.map3
import cats.kernel.Monoid
import cats.{Foldable, Functor, Semigroupal}
import org.scalatest.funsuite.AnyFunSuite

class Patterns04Test extends AnyFunSuite {
  sealed trait TransactionType
  case object DR extends TransactionType
  case object CR extends TransactionType

  sealed trait Currency
  case object USD extends Currency
  case object JPY extends Currency
  case object AUD extends Currency
  case object INR extends Currency

  case class Money(m: Map[Currency, BigDecimal]) {
    def toBaseCurrency: BigDecimal = ???
  }

  case class Transaction(
    txId: String,
    accountNo: String,
    date: Date,
    amount: Money,
    txnType: TransactionType,
    status: Boolean
  )

  case class Balance(money: Money)

  trait Account
  case class SavingsAccount(
    accountNo: String,
    name: String,
    rate: BigDecimal,
    openDate: Date,
    closeDate: Option[Date],
    balance: Balance
  ) extends Account

  test("[4.1] test Monoid abstractions") {

    trait Analytics[TransactionT, BalanceT, MoneyT] {
      def maxDebitOnDay(txns: List[TransactionT])(implicit m: Monoid[MoneyT]): MoneyT
      def sumBalances(balances: List[BalanceT])(implicit m: Monoid[MoneyT]): MoneyT

      def valueOf(txn: Transaction): Money = txn.amount
      def creditBalance(b: Balance): Money = b.money
    }

    object AnalyticsMonoid extends Analytics[Transaction, Balance, Money] {
      override def maxDebitOnDay(txns: List[Transaction])(implicit m: Monoid[Money]): Money =
        txns.filter(_.txnType == DR).foldLeft(m.empty) { (a, txn) =>
          m.combine(a, valueOf(txn))
        }

      override def sumBalances(balances: List[Balance])(implicit m: Monoid[Money]): Money =
        balances.foldLeft(m.empty)((a, bal) => m.combine(a, creditBalance(bal)))
    }

    implicit def moneyAdditionMonoid(): Monoid[Money] = new Monoid[Money] {
      override def empty: Money = Money(Map.empty)
      override def combine(x: Money, y: Money): Money = Money {
        (x.m.toSeq ++ y.m.toSeq).foldRight(Map.empty[Currency, BigDecimal]) {
          case ((curr, bln), acc) =>
            acc.get(curr) match {
              case Some(value) => acc + (curr -> (value + bln))
              case None        => acc + (curr -> bln)
            }
        }
      }
    }

    def mapReduce[F[_], A, B](as: F[A])(f: A => B)(implicit fd: Foldable[F], m: Monoid[B]) =
      fd.foldMap(as)(f)

    object AnalyticsFoldable extends Analytics[Transaction, Balance, Money] {
      implicit val m: Monoid[Money] = moneyAdditionMonoid()
      def maxDebitOnDay(txns: List[Transaction])(implicit m: Monoid[Money]): Money =
        mapReduce(txns.filter(_.txnType == DR))(valueOf)
      def sumBalances(bs: List[Balance])(implicit m: Monoid[Money]): Money =
        mapReduce(bs)(creditBalance)
    }

  }

  test("[4.2.2] test The Applicative Functor pattern") {

    trait Validator[V[_]] {
      def validateAccountNo(no: String): V[String]
      def validateOpenCloseDate(
        openDate: Option[Date],
        closeDate: Option[Date]
      ): V[(Date, Option[Date])]

      def validateRateOfInterest(rate: BigDecimal): V[BigDecimal]
    }

    object ValidatorImpl extends Validator[Option] {
      override def validateAccountNo(no: String): Option[String] = ???
      override def validateOpenCloseDate(
        openDate: Option[Date],
        closeDate: Option[Date]
      ): Option[(Date, Option[Date])] = ???
      override def validateRateOfInterest(rate: BigDecimal): Option[BigDecimal] = ???
    }

    def savingsAccount(
      no: String,
      name: String,
      rate: BigDecimal,
      openDate: Option[Date],
      closeDate: Option[Date],
      balance: Balance
    ): Option[Account] = {
      import ValidatorImpl._
      map3(
        validateAccountNo(no),
        validateOpenCloseDate(openDate, closeDate),
        validateRateOfInterest(rate)
      ) { (n, d, r) =>
        SavingsAccount(n, name, r, d._1, d._2, balance)
      }
    }

    def lift3[F[_], A, B, C, D](f: (A, B, C) => D)(implicit
      semigroupal: Semigroupal[F],
      functor: Functor[F]
    ): (F[A], F[B], F[C]) => F[D] =
      map3(_, _, _)(f)

    import cats.instances.option._
    def savingsAccountLift(
      name: String,
      balance: Balance
    ): (
      Option[String],
      Option[(Date, Option[Date])],
      Option[BigDecimal]
    ) => Option[SavingsAccount] = {
      lift3((n: String, d: (Date, Option[Date]), r: BigDecimal) =>
        SavingsAccount(n, name, r, d._1, d._2, balance)
      )
    }

  }

}
