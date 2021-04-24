trait Mammal
trait Dog extends Mammal
trait Cat extends Mammal

trait HashSetMut[T] {
  def add(item: T): Unit
}

trait HashSetImmut[+T] {
  def add[U >: T](item: U): HashSetImmut[U]
}

case class HSMut[X]() extends HashSetMut[X] {
  val storage: Set[X] = Set()
  override def add(item: X): Unit = storage + item
}

case class HSImmut[+X]() extends HashSetImmut[X] {
  override def add[U >: X](item: U): HSImmut[U] = null
}

val dogs = new HSImmut[Dog]
val mammals: HSImmut[Mammal] = dogs
dogs.add(new Cat {})
mammals.add(new Cat {})

val dogs2: HSMut[Dog] = new HSMut[Dog]

//<editor-fold desc="doesn't compile">
// val mammals: HSMut[Mammal] = dogs2
//</editor-fold>

dogs2.add(new Dog {})
//<editor-fold desc="doesn't compile"
// dogs2.add(new Cat {})
//</editor-fold>
dogs2
