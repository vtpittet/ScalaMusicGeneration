package generation

sealed trait Result[+A]

case class Success[A](tree: ParsingTree[A]) extends Result[A]
case class Pending[A](tree: ParsingTree[A]) extends Result[A]
case object Failure extends Result[Nothing]
