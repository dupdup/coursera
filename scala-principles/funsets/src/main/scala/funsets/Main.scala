package funsets

object Main extends App {
  import FunSets._

  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val union123: Set = union(union(s1, s2),s3)
  val c = map(union123,x=>x*2)
  println(printSet(c))

}
