package dog.shebang.english
package lexer

import lexer.data.NaturalNumber

import org.scalatest.funspec.AnyFunSpec

// --Implementation--

object Lexer:
  def apply(value: String): List[NaturalNumber] =
    val maybeNaturalNumberList = value
      .map(character => character.toIntOption.flatMap(NaturalNumber.apply))
      .toList
    val naturalNumberList = maybeNaturalNumberList.flatten

    if maybeNaturalNumberList.exists(_.isEmpty) then
      Nil
    else
      naturalNumberList
    end if
  end apply

  extension (value: Char)
    private def toIntOption: Option[Int] =
      value.toString.toIntOption
    end toIntOption
  end extension

end Lexer

// --Test--

class Lexer extends AnyFunSpec:
  import NaturalNumber.*

  describe("Lexer") {
    describe("apply") {
      describe("Empty") {
        type TestCase = (String, Nil.type)
        val testCaseList = List[TestCase](
          ("", Nil),
          ("a", Nil),
          ("1a", Nil),
          ("a1", Nil),
        )

        testCaseList.foreach { (value, expected) =>
          describe(s"when value is $value") {
            it(s"should return $expected") {
              assert(Lexer(value) == expected)
            }
          }
        }
      }

      describe("Cons") {
        type TestCase = (String, List[NaturalNumber])
        val testCaseList = List[TestCase](
          ("1", List(One)),
          ("12", List(One, Two)),
          ("123", List(One, Two, Three)),
          ("1230", List(One, Two, Three, Zero)),
          ("1204", List(One, Two, Zero, Four)),
          ("0123", List(Zero, One, Two, Three)),
        )

        testCaseList.foreach { (value, expected) =>
          describe(s"when value is $value") {
            it(s"should return $expected") {
              assert(Lexer(value) == expected)
            }
          }
        }
      }
    }
  }

end Lexer
