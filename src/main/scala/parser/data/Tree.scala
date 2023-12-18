package dog.shebang.english
package parser.data

import lexer.data.NaturalNumber

// --Implementation--
sealed trait Tree

object Tree:
  enum UnitNumber extends Tree:
    case Thousand(left: NoUnitNumber.Hundred, right: NoUnitNumber.Hundred) extends UnitNumber
    case Million(left: NoUnitNumber.Hundred, right: UnitNumber.Thousand) extends UnitNumber
    case Billion(left: NoUnitNumber.Hundred, right: UnitNumber.Million) extends UnitNumber
  end UnitNumber

  enum NoUnitNumber extends Tree:
    case One(naturalNumber: NaturalNumber) extends NoUnitNumber
    case Ten(left: NoUnitNumber.One, right: NoUnitNumber.One) extends NoUnitNumber
    case Hundred(left: NoUnitNumber.One, right: NoUnitNumber.Ten) extends NoUnitNumber
  end NoUnitNumber
end Tree
