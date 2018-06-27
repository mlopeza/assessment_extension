package assessment

import cats.data._
import cats.implicits._

import scala.util.matching.Regex

case class Time(day: Int, hour: Int, minute: Int, midday: String)

object TimeValidation {

  type ValidationResult[A] = ValidatedNel[Throwable, A]

  val validationRegex: Regex = """([0-7]) ([0-1]?[0-9]):([0-5]?[0-9]) (AM|PM)""".r

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
    time match {
      case validationRegex(dayPart, hourPart, minutePart, middayPart) =>
        Time(dayPart.toInt, hourPart.toInt, minutePart.toInt, middayPart).validNel
      case _ => new Exception(s"""Time "$time" is not in the format "d [H]H:mm {AM|PM}"""").invalidNel
    }
}

object MinuteAdder {

  import TimeValidation._

  private val MinutesHour = 60
  private val MinutesDay = MinutesHour * 24
  private val MinutesWeek = MinutesDay * 7

  // This is just to prevent repetition later on.
  def baseChange(value: Int, base: Int): Int =
    if (value < 0) base + (value % base) else value % base

  // This returns a Validation that is either the result of the operation, or a non empty list of Exceptions.
  def addMinutes(time: String, minutesToAdd: Int): ValidationResult[String] = {
    validateTime(time).map {
      case Time(day, hour, minute, midday) =>
        // convert the time to minutes since the first day of the week to make later calculations more obvious
        val minutesSinceMonday = (
          minute
          + ((hour % 12) * MinutesHour)
          + (day * MinutesDay)
          + (if (midday == "PM") 12 * MinutesHour else 0))

        // Normalize minutes after applying the minutesAdd
        val rawMinutes = baseChange(minutesSinceMonday + minutesToAdd, MinutesWeek)

        // figure out the new minute part
        val newMinutes = baseChange(rawMinutes, MinutesHour)

        // figure out the new hour part
        val rawHours = (rawMinutes % MinutesDay) / MinutesHour
        val newHours12 = if (rawHours % 12 == 0) 12 else rawHours % 12

        // figure out the day part
        val newDay = rawMinutes / MinutesDay

        // figure out the new midday part
        val newMidday = if (rawHours >= 12) "PM" else "AM"

        f"$newDay $newHours12:$newMinutes%02d $newMidday"
    }
  }

  def addMinutes(time: String, minutesToAdd: String): ValidationResult[String] =
    validateInt(minutesToAdd).andThen {
      minutes => addMinutes(time, minutes)
    }
}

object MinuteAdderApplication extends App {
  override def main(args: Array[String]): Unit = {
    if (args.length > 2) println("Too many arguments!")
    else {
      val result = MinuteAdder.addMinutes(args(0), args(1)).leftMap { nel =>
        "The following errors were encountered:\n" +
          nel.toList.mkString("\n")
      }
      println(result)
    }
  }
}
