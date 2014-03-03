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