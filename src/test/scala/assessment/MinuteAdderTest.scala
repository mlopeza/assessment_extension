package assessment

import org.scalatest._

class MinuteAdderTest extends FunSpec with Matchers with EitherValues {

  describe("MinuteAdder") {
    describe("Positive Path") {
      it("should add minutes to a simple time") {
        MinuteAdder.addMinutes("9:13 AM", 10).toEither.right.value should be("9:23 AM")
      }
      it("should add minutes that increment the hour") {
        MinuteAdder.addMinutes("9:13 AM", 70).toEither.right.value should be("10:23 AM")
      }
      it("should add minutes that change AM and PM") {
        MinuteAdder.addMinutes("9:13 AM", 200).toEither.right.value should be("12:33 PM")
        MinuteAdder.addMinutes("9:13 PM", 200).toEither.right.value should be("12:33 AM")
      }
      it("should subtract negative minutes") {
        MinuteAdder.addMinutes("9:13 AM", -10).toEither.right.value should be("9:03 AM")
      }
      it("should add minutes that decrement the hour with negative minutes") {
        MinuteAdder.addMinutes("9:13 AM", -200).toEither.right.value should be("5:53 AM")
      }
      it("should add minutes that change AM and PM with negative minutes") {
        MinuteAdder.addMinutes("9:13 AM", -720).toEither.right.value should be("9:13 PM")
        MinuteAdder.addMinutes("9:13 PM", -720).toEither.right.value should be("9:13 AM")
      }
      it("should add more than a day's worth of minutes") {
        MinuteAdder.addMinutes("9:13 AM", 1500).toEither.right.value should be("10:13 AM")
      }
      it("should subtract more than a day's worth of minutes") {
        MinuteAdder.addMinutes("9:13 AM", -1500).toEither.right.value should be("8:13 AM")
      }
      it("add minutes correctly near midnight and noon") {
        MinuteAdder.addMinutes("12:00 AM", 10).toEither.right.value should be("12:10 AM")
        MinuteAdder.addMinutes("12:00 PM", 10).toEither.right.value should be("12:10 PM")
        MinuteAdder.addMinutes("12:00 AM", -10).toEither.right.value should be("11:50 PM")
        MinuteAdder.addMinutes("12:00 PM", -10).toEither.right.value should be("11:50 AM")
      }
    }

    describe("Negative Path") {
      it("should fail on an incorrect time") {
        MinuteAdder.addMinutes("19:75 Q", 200).toEither.left.value.head.getMessage should be("Time \"19:75 Q\" is not in the format \"[H]H:mm {AM|PM}\"")
      }
      it("should fail on an invalid time string") {
        MinuteAdder.addMinutes("half past 5", 200).toEither.left.value.head.getMessage should be("Time \"half past 5\" is not in the format \"[H]H:mm {AM|PM}\"")
      }
    }
  }

  describe("TimeValidation") {
    describe("validateInt") {
      it("should validate an Int") {
        TimeValidation.validateInt("10").toEither.right.value should be(10)
      }
      it("should fail gracefully") {
        TimeValidation.validateInt("Q").toEither.left.value.head.getMessage should be("For input string: \"Q\"")
      }
    }
    describe("validateTime") {
      it("should validate a time") {
        TimeValidation.validateTime("12:00 AM").toEither.right.value should be(Time(12, 0, "AM"))
        TimeValidation.validateTime("9:13 PM").toEither.right.value should be(Time(9, 13, "PM"))
      }
      it("should fail gracefully") {
        val r = TimeValidation.validateTime("15:90 Z").toEither.left.value
        r.length should be(1)
        r.head.getMessage should be("Time \"15:90 Z\" is not in the format \"[H]H:mm {AM|PM}\"")
      }
    }
  }

}
