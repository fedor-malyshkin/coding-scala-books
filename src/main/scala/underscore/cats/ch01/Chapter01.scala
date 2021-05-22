package underscore.cats.ch01

object Chapter01 {

  def compareInts(): Boolean = {
    import cats.implicits._

    (123 === 123) && (123 =!= 234)
  }

}
