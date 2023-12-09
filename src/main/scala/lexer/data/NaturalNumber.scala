package dog.shebang.english
package lexer.data

import org.scalatest.funspec.AnyFunSpec

// --Implementation--

enum NaturalNumber:
  case Zero extends NaturalNumber
  case One extends NaturalNumber
  case Two extends NaturalNumber
  case Three extends NaturalNumber
  case Four extends NaturalNumber
  case Five extends NaturalNumber
  case Six extends NaturalNumber
  case Seven extends NaturalNumber
  case Eight extends NaturalNumber
  case Nine extends NaturalNumber
end NaturalNumber

object NaturalNumber:
  def apply(n: Int): Option[NaturalNumber] = n match
    case 0 => Some(Zero)
    case 1 => Some(One)
    case 2 => Some(Two)
    case 3 => Some(Three)
    case 4 => Some(Four)
    case 5 => Some(Five)
    case 6 => Some(Six)
    case 7 => Some(Seven)
    case 8 => Some(Eight)
    case 9 => Some(Nine)
    case _ => None
  end apply

end NaturalNumber

// --Test--
class NaturalNumberTest extends AnyFunSpec:
  describe("NaturalNumberTest") {
    describe("apply") {
      type TestCase = (Int, Option[NaturalNumber])
      val testCaseList = List[TestCase](
        (0, Some(NaturalNumber.Zero)),
        (1, Some(NaturalNumber.One)),
        (2, Some(NaturalNumber.Two)),
        (3, Some(NaturalNumber.Three)),
        (4, Some(NaturalNumber.Four)),
        (5, Some(NaturalNumber.Five)),
        (6, Some(NaturalNumber.Six)),
        (7, Some(NaturalNumber.Seven)),
        (8, Some(NaturalNumber.Eight)),
        (9, Some(NaturalNumber.Nine)),
        (10, None)
      )

      testCaseList.foreach { case (input, expected) =>
        describe(s"when $input is given") {
          it(s"should return $expected") {
            assert(NaturalNumber(input) == expected)
          }
        }
      }
    }
  }
end NaturalNumberTest
