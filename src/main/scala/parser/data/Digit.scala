package dog.shebang.english
package parser.data

enum Digit:
  case Thousand extends Digit
  case Million extends Digit
  case Billion extends Digit
end Digit

object Digit:
  def apply(digit: Int): Option[Digit] = digit match
    case 3 => Some(Thousand)
    case 6 => Some(Million)
    case 9 => Some(Billion)
    case _ => None
  end apply
  
end Digit
  
