package sandBox

object ShortCuts extends App {
  val f = FactoryWithExtremlyLongAndAnnoyingNameHoweverUsedALotInTheCode
  println(f(3))
  println(f(5))
  
  println(f(10))
  
  
//  for(i <- 0 to 100) println(f(i*i/50))
  
}

object FactoryWithExtremlyLongAndAnnoyingNameHoweverUsedALotInTheCode {
  def apply(arg: Int): String = if(arg>0) "*" + apply(arg-1) else ""
}


case class TestTransfList[T](pred: PartialFunction[Any, T], funs: List[TestTransf[T]]) {
  def + (fun: T => Any, period: Int = 1, from: Int = 0, to: Int = -1): TestTransfList[T] = {
    TestTransfList(pred, TestTransf(fun, period, from, to) :: funs)
  }
}
object TestTransfList {
  implicit def partToEmptyTransfs[T](pred: PartialFunction[Any, T]): TestTransfList[T] = {
    TestTransfList(pred, Nil)
  }
}

case class TestTransf[T](apply: T => Any, period: Int, from: Int, to: Int)

object Test {
  implicit def partToEmptyTransfs[T](pred: PartialFunction[Any, T]): TestTransfList[T] = {
    TestTransfList(pred, Nil)
  }
  val startpf: PartialFunction[Any, Boolean] = {case _ => false}
  def useTTL(ttl: TestTransfList[Boolean]): Unit = {}
  
  useTTL(startpf + (!_) + (if (_) 1 else 0) + (!_, 2, 4, 7))
}