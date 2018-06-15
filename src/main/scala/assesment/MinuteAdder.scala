package assessment

import cats.data._
import cats.implicits._

import scala.util.matching.Regex

case class Time(hour: Int, minute: Int, midday: String)

object TimeValidation {

  type ValidationResult[A] = ValidatedNel[Throwable, A]

  val validationString = """([0-1]?[0-9]):([0-5]?[0-9]) (AM|PM)"""
  val validationRegex: Regex = validationString.r

  // This is a handy way to catch errors from converting a string to an int.
  def validateInt(s: String): ValidationResult[Int] = {
    try {
      s.toInt.validNel
    } catch {
      case nfe: NumberFormatException => nfe.invalidNel
    }
  }

  // This validates that the given string is in the right format, parses the time parts, and returns a Time.
  def validateTime(time: String): ValidationResult[Time] =
    if (time.matches(validationString)) {
      time match {
        case validationRegex(hourPart, minutePart, middayPart) =>
          (validateInt(hourPart), validateInt(minutePart)).mapN(Time(_, _, middayPart))
      }
    } else new Exception(s"""Time "$time" is not in the format "[H]H:mm {AM|PM}"""").invalidNel
}

object MinuteAdder {
  import TimeValidation._

  // This is just to prevent repetition later on.
  def baseChange(value: Int, base: Int): Int =
    if(value < 0) base + (value % base) else value % base

  // This returns a Validation that is either the result of the operation, or a non empty list of Exceptions.
  def addMinutes(time: String, minutesToAdd: Int): ValidationResult[String] = {
    validateTime(time).map{ case Time(hour, minute, midday) =>
      // convert the time to minutes since midnight to make later calculations more obvious
      val minutesSinceMidnight = minute + ((hour % 12) * 60) + (if(midday == "PM") 12 * 60 else 0)

      // figure out the new minute part
      val rawMinutes = minutesSinceMidnight + minutesToAdd
      val newMinutes = baseChange(rawMinutes, 60)

      // figure out the new hour part
      val rawHours = rawMinutes / 60 - (if(rawMinutes < 0) 1 else 0)
      val newHours24 = baseChange(rawHours, 24)
      val newHours12 = if(newHours24 % 12 == 0) 12 else newHours24 % 12

      // figure out the new midday part
      val newMidday = if(newHours24 >= 12) "PM" else "AM"

      f"$newHours12:$newMinutes%02d $newMidday"
    }
  }

  def addMinutes(time: String, minutesToAdd: String): ValidationResult[String] =
    validateInt(minutesToAdd).andThen{ minutes =>
      addMinutes(time, minutes)
    }
}

object MinuteAdderApplication extends App {
  override def main(args: Array[String]): Unit = {
    if(args.length > 2) println("Too many arguments!")
    else {
      val result = MinuteAdder.addMinutes(args(0), args(1)).leftMap{nel =>
        "The following errors were encountered:\n" +
        nel.toList.mkString("\n")
      }

      println(result)
    }
  }
}