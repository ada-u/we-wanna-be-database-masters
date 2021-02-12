import scala.util.matching.Regex

object MetaCommand {

  sealed trait Result

  case object Success extends Result

  case object UnrecognizedCommand extends Result

  val regexp: Regex = """\.(.+)""".r

  def execute: String => Result = {
    case "exit" =>
      System.exit(0)
      Success
    case _ =>
      UnrecognizedCommand
  }

}
