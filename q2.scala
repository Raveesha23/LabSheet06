import scala.io.StdIn._

object StudentRecords extends App {

    def validateInput(name: String, marks: String, totalMarks: String): (Boolean, Option[String]) = {
        if (name.isEmpty) {
            (false, Some("Name cannot be empty."))
        } else {
            try {
                val marksInt = marks.toInt
                val totalMarksInt = totalMarks.toInt
                if (marksInt < 0 || marksInt > totalMarksInt) {
                (false, Some("Marks should be positive and not exceed total possible marks."))
                } else {
                (true, None)
                }
            } catch {
                case _: NumberFormatException => (false, Some("Marks and total possible marks should be integers."))
            }
        }
    }

    def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
        var valid = false
        var name = ""
        var marks = ""
        var totalMarks = ""

        while (!valid) {
            println("Enter student name:")
            name = readLine()

            println("Enter student marks:")
            marks = readLine()

            println("Enter total possible marks:")
            totalMarks = readLine()

            val (isValid, errorMessage) = validateInput(name, marks, totalMarks)

            if (isValid) {
                valid = true
            } else {
                println(s"Invalid input: ${errorMessage.getOrElse("Unknown error")}")
            }
        }

        val marksInt = marks.toInt
        val totalMarksInt = totalMarks.toInt
        val percentage = (marksInt.toDouble / totalMarksInt) * 100

        val grade = percentage match {
            case p if p >= 90 => 'A'
            case p if p >= 75 => 'B'
            case p if p >= 50 => 'C'
            case _ => 'D'
        }

        (name, marksInt, totalMarksInt, percentage, grade)
    }

    def printStudentRecord(student: (String, Int, Int, Double, Char)): Unit = {
        val (name, marks, totalMarks, percentage, grade) = student

        println("-----------------------------------------")
        println(s"Student Name: $name")
        println(s"Marks: $marks / $totalMarks")
        println(s"Percentage: $percentage%")
        println(s"Grade: $grade")
    }

    val studentInfo = getStudentInfoWithRetry()
    printStudentRecord(studentInfo)
}
