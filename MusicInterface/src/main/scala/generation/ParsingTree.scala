package generation

case class ParsingTree[A](
  rTree: List[PrefixOperator],
  stack: List[Todo[A]],
  probability: Double) { self =>

}
