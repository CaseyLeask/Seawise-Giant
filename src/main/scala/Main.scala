object Main {
  def main(args: Array[String]) = args match {
    case Array(x: String, _*) => InputParsing.ReadingFromFile.Read(x)
    case _ => InputParsing.ConsoleInteraction.begin()
  }
}