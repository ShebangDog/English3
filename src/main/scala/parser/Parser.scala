package dog.shebang.english
package parser

import lexer.data.NaturalNumber
import parser.data.Tree.{NoUnitNumber, UnitNumber}
import parser.data.{Digit, Tree}

import org.scalatest.funspec.AnyFunSpec

import scala.annotation.tailrec

// --Implementation--
object Parser:
  def apply(naturalNumberList: List[NaturalNumber]): Option[Tree] = naturalNumberList match
    case NaturalNumber.Zero :: _ :: _ => None
    case _ => number(naturalNumberList)
  end apply

  private def toDigit(list: List[NaturalNumber]): Either[(Int, Option[Digit]), Digit] = makeDigit(list.length - 1)

  @tailrec
  private def makeDigit(digit: Int, sub: Int = 0): Either[(Int, Option[Digit]), Digit] = Digit(digit) match
    case _ if digit == 0 => Left((sub, None))
    case Some(digit) if sub != 0 => Left((sub, Some(digit)))
    case Some(digit) => Right(digit)
    case None => makeDigit(digit - 1, sub + 1)
  end makeDigit

  private def asTen(one: NoUnitNumber.One): NoUnitNumber.Ten = NoUnitNumber.Ten(
    NoUnitNumber.One(NaturalNumber.Zero),
    one,
  )

  private def asHundred(noUnitNumber: NoUnitNumber.Ten): NoUnitNumber.Hundred = NoUnitNumber.Hundred(
    NoUnitNumber.One(NaturalNumber.Zero),
    noUnitNumber,
  )

  private def toHundred(noUnitNumber: NoUnitNumber): NoUnitNumber.Hundred = noUnitNumber match
    case one: NoUnitNumber.One => asHundred(asTen(one))
    case ten: NoUnitNumber.Ten => asHundred(ten)
    case hundred: NoUnitNumber.Hundred => hundred

  private def number(naturalNumberList: List[NaturalNumber]): Option[Tree] = toDigit(naturalNumberList) match
    case Left((_, None)) => noUnitNumber(naturalNumberList)
    case Left((count, Some(digit))) =>
      val (first, second) = naturalNumberList.splitAt(count + 1)
      unitNumber(first, second, digit)
    case Right(digit) => unitNumber(naturalNumberList, digit)
  end number

  private def unitNumber(naturalNumberList: List[NaturalNumber], digit: Digit): Option[Tree] = digit match
    case Digit.Thousand => thousand(naturalNumberList)
    case Digit.Million => million(naturalNumberList)
    case Digit.Billion => billion(naturalNumberList)
  end unitNumber

  private def unitNumber(firstNaturalNumberList: List[NaturalNumber], secondNaturalNumberList: List[NaturalNumber], digit: Digit): Option[Tree] =
    val maybeHundred = noUnitNumber(firstNaturalNumberList).map(toHundred)

    for
      left <- maybeHundred
      right <- unitNumber(NaturalNumber.Zero :: secondNaturalNumberList, digit)
    yield
      right match
        case thousand: Tree.UnitNumber.Thousand => Tree.UnitNumber.Million(left, thousand)
        case million: Tree.NoUnitNumber.Hundred => Tree.UnitNumber.Thousand(left, million)
      end match

  end unitNumber

  private def billion(naturalNumberList: List[NaturalNumber]): Option[Tree] = None

  private def million(naturalNumberList: List[NaturalNumber]): Option[Tree] = None
  end million

  private def thousand(naturalNumberList: List[NaturalNumber]): Option[Tree.UnitNumber.Thousand] =
    println(naturalNumberList)
    naturalNumberList match
      case first :: second :: third :: fourth :: Nil => Some(
        Tree.UnitNumber.Thousand(
          hundred(NaturalNumber.Zero, NaturalNumber.Zero, first),
          hundred(second, third, fourth)
        )
      )
      case _ => None
    end match
  end thousand

  private def noUnitNumber(naturalNumberList: List[NaturalNumber]): Option[Tree.NoUnitNumber] = naturalNumberList match
    case first :: Nil => Some(one(first))
    case first :: second :: Nil => Some(ten(first, second))
    case first :: second :: third :: Nil => Some(hundred(first, second, third))
    case Nil => None
    case _ :: _ => None
  end noUnitNumber

  private def hundred(first: NaturalNumber, second: NaturalNumber, third: NaturalNumber): Tree.NoUnitNumber.Hundred = Tree.NoUnitNumber.Hundred(
    Tree.NoUnitNumber.One(first),
    Tree.NoUnitNumber.Ten(Tree.NoUnitNumber.One(second), Tree.NoUnitNumber.One(third))
  )

  private def ten(first: NaturalNumber, second: NaturalNumber): Tree.NoUnitNumber.Ten =
    Tree.NoUnitNumber.Ten(Tree.NoUnitNumber.One(first), Tree.NoUnitNumber.One(second))
  end ten

  private def one(value: NaturalNumber): Tree.NoUnitNumber.One = Tree.NoUnitNumber.One(value)
end Parser

// --Test--

class Parser extends AnyFunSpec:

  import NaturalNumber.*

  describe("Parser") {
    describe("apply") {
      describe("None") {
        type TestCase = (List[NaturalNumber], Nil.type)
        val testCaseList = List[TestCase](
          (Nil, Nil),
          (List(Zero, One), Nil),
          (List(Zero, Zero), Nil)
        )

        testCaseList.foreach { case (input, expected) =>

          describe(s"when the input is $input") {
            it(s"should return $expected") {
              assert(Parser(input).isEmpty)
            }
          }
        }
      }

      describe("Some") {
        type TestCase = (List[NaturalNumber], Tree)
        val testCaseList = List[TestCase](
//          (List(Zero), Tree.NoUnitNumber.One(Zero)),
//          (List(One), Tree.NoUnitNumber.One(One)),
//          (List(One, Zero), Tree.NoUnitNumber.Ten(Tree.NoUnitNumber.One(One), Tree.NoUnitNumber.One(Zero))),
//          (List(One, Two), Tree.NoUnitNumber.Ten(Tree.NoUnitNumber.One(One), Tree.NoUnitNumber.One(Two))),
//          (List(One, Zero, Zero), Tree.NoUnitNumber.Hundred(Tree.NoUnitNumber.One(One), Tree.NoUnitNumber.Ten(Tree.NoUnitNumber.One(Zero), Tree.NoUnitNumber.One(Zero)))),
//          (List(One, One, Zero), Tree.NoUnitNumber.Hundred(Tree.NoUnitNumber.One(One), Tree.NoUnitNumber.Ten(Tree.NoUnitNumber.One(One), Tree.NoUnitNumber.One(Zero)))),
//          (
//            List(One, Zero, Zero, Zero),
//            Tree.UnitNumber.Thousand(
//              Tree.NoUnitNumber.Hundred(Tree.NoUnitNumber.One(Zero), Tree.NoUnitNumber.Ten(Tree.NoUnitNumber.One(Zero), Tree.NoUnitNumber.One(One))),
//              Tree.NoUnitNumber.Hundred(Tree.NoUnitNumber.One(Zero), Tree.NoUnitNumber.Ten(Tree.NoUnitNumber.One(Zero), Tree.NoUnitNumber.One(Zero)))
//            )
//          ),
//          (
//            List(One, One, Zero, Zero),
//            Tree.UnitNumber.Thousand(
//              Tree.NoUnitNumber.Hundred(Tree.NoUnitNumber.One(Zero), Tree.NoUnitNumber.Ten(Tree.NoUnitNumber.One(Zero), Tree.NoUnitNumber.One(One))),
//              Tree.NoUnitNumber.Hundred(Tree.NoUnitNumber.One(One), Tree.NoUnitNumber.Ten(Tree.NoUnitNumber.One(Zero), Tree.NoUnitNumber.One(Zero)))
//            )
//          ),
          (
            List(One, One, One, Zero, Zero),
            Tree.UnitNumber.Thousand(
              Tree.NoUnitNumber.Hundred(Tree.NoUnitNumber.One(Zero), Tree.NoUnitNumber.Ten(Tree.NoUnitNumber.One(One), Tree.NoUnitNumber.One(One))),
              Tree.NoUnitNumber.Hundred(Tree.NoUnitNumber.One(One), Tree.NoUnitNumber.Ten(Tree.NoUnitNumber.One(Zero), Tree.NoUnitNumber.One(Zero)))
            )
          ),
//          (
//            List(One, Two, Three, Four, Five, Six),
//            Tree.UnitNumber.Thousand(
//              Tree.NoUnitNumber.Hundred(
//                Tree.NoUnitNumber.One(One),
//                Tree.NoUnitNumber.Ten(Tree.NoUnitNumber.One(Two), Tree.NoUnitNumber.One(Three))
//              ),
//              Tree.NoUnitNumber.Hundred(
//                Tree.NoUnitNumber.One(Four),
//                Tree.NoUnitNumber.Ten(Tree.NoUnitNumber.One(Five), Tree.NoUnitNumber.One(Six))
//              ),
//            )
//          ),
        )

        testCaseList.foreach { testCase =>
          val (input, expected) = testCase
          describe(s"when the input is $input") {
            it(s"should return Some($expected)") {
              assert(Parser(input).contains(expected))
            }
          }
        }
      }
    }

    describe("toDigit") {
      describe("Left") {
        type TestCase = (List[NaturalNumber], (Int, Option[Digit]))
        val testCaseList = List[TestCase](
          ((List(One, Two, Three, Four, Five), (1, Some(Digit.Thousand))))
        )

        testCaseList.foreach { testCase =>
          val (input, expected) = testCase
          describe(s"when the input is $input") {
            it(s"should return Left($expected)") {
              assert(Parser.toDigit(input) == Left(expected))
            }
          }
        }
      }
    }
  }
end Parser

