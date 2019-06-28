import org.scalatest.Matchers

class ParserTest extends org.scalatest.FunSuite with Matchers {

  test("should parse and int") {
    Parser.parseInt("1") shouldBe Some(1)
  }

  test("should return none for invalid number") {
    Parser.parseInt("invalid") shouldBe None
  }

  test("should return correct fib sequence") {
    Parser.fibonacci(5) shouldBe List(0, 1, 1, 2, 3)
  }

  test("should return list of numbers with valid input") {
    Parser.transform(List("1", "2", "3")) shouldBe List(1, 2, 3)
  }

  test("should return empty list when input invalid") {
    Parser.transform(List("1", "two", "3")) shouldBe empty
  }

}
