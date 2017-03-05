import org.scalatest.FeatureSpec

class LatinSpec extends FeatureSpec {

  feature("Convert Arabic numerals to Latin Strings") {
    scenario("1 is I") {
       val latin1 = RomanNumeralConverter.arabicToLatin(1)
       assert(latin1 == "I")
    }

    scenario("2 is II") {
      val latin2 = RomanNumeralConverter.arabicToLatin(2)
      assert(latin2 == "II")
    }

    scenario("5 is V") {
      val latin5 = RomanNumeralConverter.arabicToLatin(5)
      assert(latin5 == "V")
    }

    scenario("6 is VI") {
      val latin6 = RomanNumeralConverter.arabicToLatin(6)
      assert(latin6 == "VI")
    }

    scenario("10 is X") {
      val latin10 = RomanNumeralConverter.arabicToLatin(10)
      assert(latin10 == "X")
    }

    scenario("50 is L") {
      val latin50 = RomanNumeralConverter.arabicToLatin(50)
      assert(latin50 == "L")
    }

    scenario("100 is C") {
      val latin100 = RomanNumeralConverter.arabicToLatin(100)
      assert(latin100 == "C")
    }

    scenario("500 is D") {
      val latin500 = RomanNumeralConverter.arabicToLatin(500)
      assert(latin500 == "D")
    }

    scenario("1000 is M") {
      val latin1000 = RomanNumeralConverter.arabicToLatin(1000)
      assert(latin1000 == "M")
    }

    //Implement boundary checking for 3 in a row rule
    scenario("4 is IV") {
      val latin4 = RomanNumeralConverter.arabicToLatin(4)
      assert(latin4 == "IV")
    }

    scenario("9 is IX") {
      val latin9 = RomanNumeralConverter.arabicToLatin(9)
      assert(latin9 == "IX")
    }

    scenario("90 is XC") {
      val latin90 = RomanNumeralConverter.arabicToLatin(90)
      assert(latin90 == "XC")
    }

    scenario("400 is CD") {
      val latin400 = RomanNumeralConverter.arabicToLatin(400)
      assert(latin400 == "CD")
    }

    scenario("80 is LXXX") {
      val latin80 = RomanNumeralConverter.arabicToLatin(80)
      assert(latin80 == "LXXX")
    }
  }

  feature("Convert Latin Strings to Arabic numerals") {
    scenario("I is 1") {
      val latin1 = RomanNumeralConverter.latinToArabic("I")
      assert(latin1 == 1)
    }

    scenario("D is 500") {
      val latin500 = RomanNumeralConverter.latinToArabic("D")
      assert(latin500 == 500)
    }

    scenario("IV is 4") {
      val latin4 = RomanNumeralConverter.latinToArabic("IV")
      assert(latin4 == 4)
    }

    scenario("LXXX is 80") {
      val latin80 = RomanNumeralConverter.latinToArabic("LXXX")
      assert(latin80 == 80)
    }

    scenario("XC is 90") {
      val latin90 = RomanNumeralConverter.latinToArabic("XC")
      assert(latin90 == 90)
    }
  }
}