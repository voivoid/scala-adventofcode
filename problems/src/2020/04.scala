package adventOfCode.problems
package year2020

object problem04 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, checkPassportHasAllFields)
  }

  override def solve2(input: Input): Int = {
    solve(input, isValidPassport)
  }

  import fastparse._
  import NoWhitespace._
  import adventOfCode.utils.parse.{parseValue, isParsed, num, digit}

  private def solve(input: Input, checkPassport: Passport => Boolean): Int = {
    val passports = parsePassports(input.mkString)
    passports.count(checkPassport)
  }

  private type Passport = Map[String, String]

  private[problems] def parsePassports(input: String): Seq[Passport] = {
    def key[_: P] = P(CharIn("a-z").rep(3)).!
    def value[_: P] = P(CharIn("a-z0-9#").rep).!
    def kv[_: P] = P(key ~ ":" ~ value)
    def passport[_: P] = kv.rep(sep = CharIn(" \n")).map(_.toMap)
    def passports[_: P] = P(passport.rep(sep = "\n\n"))
    def parser[_: P] = P(passports)

    parseValue(input, parser(_))
  }

  private def BYR = "byr"
  private def IYR = "iyr"
  private def EYR = "eyr"
  private def HGT = "hgt"
  private def HCL = "hcl"
  private def ECL = "ecl"
  private def PID = "pid"

  private def checkPassportHasAllFields(passport: Passport): Boolean = {
    val requiredFields = Iterator(BYR, IYR, EYR, HGT, HCL, ECL, PID)
    val passportFields = passport.keys.toSet

    requiredFields.forall(passportFields.contains)
  }

  private def isValidPassport(passport: Passport): Boolean = {
    checkPassportHasAllFields(passport) &&
    isBYRvalid(passport(BYR)) &&
    isIYRvalid(passport(IYR)) &&
    isEYRvalid(passport(EYR)) &&
    isHGTvalid(passport(HGT)) &&
    isHCLvalid(passport(HCL)) &&
    isECLvalid(passport(ECL)) &&
    isPIDvalid(passport(PID))
  }

  private[problems] def isBYRvalid(byr: String): Boolean = isYearInRange(byr, 1920, 2002)
  private[problems] def isIYRvalid(iyr: String): Boolean = isYearInRange(iyr, 2010, 2020)
  private[problems] def isEYRvalid(eyr: String): Boolean = isYearInRange(eyr, 2020, 2030)

  private[problems] def isHGTvalid(hgt: String): Boolean = {
    def heightParser[_: P] =
      P(num ~ "cm").filter(h => h >= 150 && h <= 193) |
        P(num ~ "in").filter(h => h >= 59 && h <= 76)

    isParsed(hgt, heightParser(_))
  }

  private[problems] def isHCLvalid(hcl: String): Boolean = {
    def hairColorParser[_: P] = P("#" ~ CharIn("0-9a-f").rep(6))
    isParsed(hcl, hairColorParser(_))
  }

  private[problems] def isECLvalid(ecl: String): Boolean = {
    val validECLs = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    validECLs.contains(ecl)
  }

  private[problems] def isPIDvalid(pid: String): Boolean = pid.size == 9 && pid.forall(_.isDigit)

  private def isYearInRange(field: String, from: Int, to: Int): Boolean = {
    def dateParser[_: P] = P(digit.rep(4)).!.map(_.toInt).filter(n => n >= from && n <= to)
    isParsed(field, dateParser(_))
  }

}
