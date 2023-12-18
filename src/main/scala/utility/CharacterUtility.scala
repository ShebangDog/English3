package dog.shebang.english
package utility

import org.scalatest.funspec.AnyFunSpec

object CharacterUtility:
  def toIntOption(value: Char): Option[Int] = value.toString.toIntOption
end CharacterUtility

class CharacterUtility extends AnyFunSpec:
  describe("toIntOption") {
    it("should return Some(1) when value is '1'") {
      assert(CharacterUtility.toIntOption('1').contains(1))
    }

    it("should return None when value is 'a'") {
      assert(CharacterUtility.toIntOption('a').isEmpty)
    }
  }
end CharacterUtility

