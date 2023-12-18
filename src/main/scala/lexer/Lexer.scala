package dog.shebang.english
package lexer

import lexer.data.NaturalNumber

// --Implementation--

object Lexer:
  def apply(value: String): List[NaturalNumber] =
    val maybeNaturalNumberList = value.toList.map(toNaturalNumber)
    val naturalNumberList = maybeNaturalNumberList.flatten

    maybeNaturalNumberList.find(_.isEmpty) match
      case Some(_) => Nil
      case None => naturalNumberList
    end match
  end apply

  def toNaturalNumber(value: Char): Option[NaturalNumber] =
    toIntOption(value).flatMap(NaturalNumber.apply)

  private def toIntOption(value: Char): Option[Int] = value.toString.toIntOption

end Lexer

// --Test--

import org.scalatest.funspec.AnyFunSpec
class LexerTest extends AnyFunSpec:
  import NaturalNumber.*
  import lexer.Lexer.*

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
            ignore(s"should return $expected") {
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
            ignore(s"should return $expected") {
              assert(Lexer(value) == expected)
            }
          }
        }
      }
    }

    describe("toNaturalNumber") {
      describe("None") {
        type TestCase = (Char, None.type)
        val testCaseList = List[TestCase](
          ('a', None),
          ('A', None),
          ('あ', None),
          ('ア', None),
        )

        testCaseList.foreach { (value, expected) =>
          describe(s"when value is $value") {
            it(s"should return $expected") {
              assert(Lexer.toNaturalNumber(value) == expected)
            }
          }
        }
      }

      describe("Some") {
        type TestCase = (Char, Some[NaturalNumber])
        val testCaseList = List[TestCase](
          ('0', Some(Zero)),
          ('1', Some(One)),
          ('2', Some(Two)),
          ('3', Some(Three)),
          ('4', Some(Four)),
          ('5', Some(Five)),
          ('6', Some(Six)),
          ('7', Some(Seven)),
          ('8', Some(Eight)),
          ('9', Some(Nine)),
        )

        testCaseList.foreach { (value, expected) =>
          describe(s"when value is $value") {
            it(s"should return $expected") {
              assert(Lexer.toNaturalNumber(value) == expected)
            }
          }
        }
      }
    }
  }

end LexerTest
