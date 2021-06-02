package frdomain.ch03.lenses

import java.util.Date

import scala.collection.mutable
import scala.util.{Success, Try}

import cats.data.Reader
import monocle.macros.GenIso
import monocle.{Focus, Iso, Lens}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Lenses03Test extends AnyFunSuite {
  type Price = BigInt
  case class Address(no: String, street: String, city: String, state: String, zip: String)
  case class Customer(id: Int, name: String, address: Address)
  case class Item(id: Int, name: String, price: Price)
  case class Position(item: Item, amount: Int)
  case class Order(customer: Customer, positions: List[Position], delivery: Option[Address])

  val address1 = Address("12", "Big Street", "Richmond", "TXT", "123456")
  val address2 = Address("14", "Small Street", "Winsboro", "DSC", "098765")
  val customer = Customer(1, "Bob", address1)
  val item1 = Item(1, "Bread", 120)
  val item2 = Item(2, "Butter", 110)
  val pos1 = Position(item1, 2)
  val pos2 = Position(item2, 3)
  val pos3 = Position(item2, 33)
  val order1 = Order(customer, List(pos1), None)
  val order2 = Order(customer, List(pos1, pos2), Some(address2))

  test("[3.2] test Monocle: focus") {
    import monocle.macros.syntax.all._

    val updatedOrder11 = order1
      .focus(_.customer.name)
      .replace("Bob")
    updatedOrder11.customer.name should be("Bob")

    def replaceDeliveryStreet(order: Order, deliveryStreet: String): Order = {
      order
        .focus(_.delivery)
        .some
        .andThen(Focus[Address](_.street))
        .replace(deliveryStreet)
    }
    replaceDeliveryStreet(order1, "Medium Street").delivery.isEmpty should be(true)
    replaceDeliveryStreet(order2, "Medium Street").delivery should be(
      Some(Address("14", "Medium Street", "Winsboro", "DSC", "098765"))
    )

    def replacePosition(order: Order, index: Int, position: Position): Order = {
      order
        .focus(_.positions)
        .index(index)
        .replace(position)
    }

    // if it doesn't exist - nothing will be replaced
    replacePosition(order1, 1, pos3).positions should be(List(pos1))
    // if it exists - it will be replaced
    replacePosition(order2, 1, pos3).positions should be(List(pos1, pos3))

  }

  test("[3.2] test Monocle: Iso") {
    val addressToTuple = Iso[Address, (String, String, String, String, String)](a =>
      (a.no, a.street, a.city, a.state, a.zip)
    ) { case (no, street, city, state, zip) =>
      Address(no, street, city, state, zip)
    }

    addressToTuple.get(address1) should be(("12", "Big Street", "Richmond", "TXT", "123456"))
    val tuple = addressToTuple.get(address1)
    addressToTuple.reverseGet(tuple) should be(
      Address("12", "Big Street", "Richmond", "TXT", "123456")
    )
    addressToTuple(tuple) should be(address1)
    addressToTuple.reverseGet(tuple) should be(address1)
  }

  test("[3.2] test Monocle: Iso macros") {
    val addressToTuple = GenIso.fields[Address]

    addressToTuple.get(address1) should be(("12", "Big Street", "Richmond", "TXT", "123456"))
    val tuple = addressToTuple.get(address1)
    addressToTuple.reverseGet(tuple) should be(
      Address("12", "Big Street", "Richmond", "TXT", "123456")
    )
    addressToTuple(tuple) should be(address1)
    addressToTuple.reverseGet(tuple) should be(address1)
  }

  test("[3.2] test Monocle: Lenses") {
    val addressToStreet = Lens[Address, String](_.street)(s => a => a.copy(street = s))
    addressToStreet.get(address1) should be("Big Street")
    val replaceMethod = addressToStreet.replace("New String")
    replaceMethod(address1) should be(Address("12", "New String", "Richmond", "TXT", "123456"))

    val replaceMethod2 = addressToStreet.modify(_.toUpperCase)
    replaceMethod2(address1) should be(Address("12", "BIG STREET", "Richmond", "TXT", "123456"))

    def modifyFun(str: String) = Option(str)
    import cats.implicits._
    val replaceMethod3: Address => Option[Address] = addressToStreet.modifyF(modifyFun)
    replaceMethod3(address1) should be(
      Some(Address("12", "Big Street", "Richmond", "TXT", "123456"))
    )
  }

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
