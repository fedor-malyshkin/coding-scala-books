class C1(private val f1: String, other: Option[C1] = None) {
  def read = for { otherV <- other } yield otherV.f1
}

val c1_1 = new C1("foo")
val c1_2 = new C1("popa", Some(c1_1))

c1_1.read
c1_2.read

//<editor-fold desc="won't compile">
//class C2(private[this ] val f1: String, other: Option[C2] = None) {
//  def read = for { otherV <- other } yield otherV.f1
//}
//</editor-fold>
