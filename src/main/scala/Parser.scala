import scala.util.Try

object Parser {

  def parseInt(input: String): Option[Int] = Try(input.toInt).toOption

  def fibonacci(num: Int): List[Int] = {
    def nthFib(num: Int, a: Int, b: Int): Int = {
      num match {
        case 0 => 0
        case 1 => b
        case _ => nthFib(num - 1, b, a + b)
      }
    }

    List.tabulate(num)(nthFib(_, 0, 1))
  }

  // all or nothing using parseInt
  def transform[A](input: List[A]): List[Int] = {
    def sequence[B](i: List[Option[B]]): Option[List[B]] = i match {
      case Nil => Some(Nil)
      case None :: _ => None
      case Some(head) :: t => sequence(t).map(tail => head :: tail)
    }

    sequence(input.map(i => parseInt(i.toString))).toList.flatten
  }

  def transformFold[A](input: List[A]): List[Int] = {
    def sequence[B](i: List[Option[B]]): Option[List[B]] = {
      i.foldLeft(Option(List[B]())){(state, current) =>
        for {
          s <- state
          c <- current
        } yield c :: s
      }.map(_.reverse)

    }
    sequence(input.map(i => parseInt(i.toString))).toList.flatten
  }
}