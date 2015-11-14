package InputParsing

import Commands._
import Direction._
import org.specs2.mutable._

class CommandParsingTest extends Specification {
  "CommandParsing" should {
    "with a PLACE command" should {
      "parse valid commands" in {
        val lines = Iterator("PLACE 1,1,NORTH")
        CommandParsing.toRobotCommands(lines).toList must be equalTo List(Place(1,1,North))
      }

      "not parse invalid x values" in {
        val lines = Iterator(
          "PLACE -1,1,NORTH",
          "PLACE 0.5,1,NORTH"
        )
        CommandParsing.toRobotCommands(lines).toList must be equalTo List()
      }

      "not parse invalid y values" in {
        val lines = Iterator(
          "PLACE 1,-1,NORTH",
          "PLACE 1,0.5,NORTH"
        )
        CommandParsing.toRobotCommands(lines).toList must be equalTo List()
      }

      "not parse invalid direction values" in {
        val lines = Iterator(
          "PLACE 1,1,WESTSIDE",
          "PLACE 1,1,north",
          "PLACE 1,1,North",
          "PLACE 1,1,NORTHS"
        )
        CommandParsing.toRobotCommands(lines).toList must be equalTo List()
      }
    }

    "parse MOVE commands" in {
      val lines = Iterator("MOVE")
      CommandParsing.toRobotCommands(lines).toList must be equalTo List(Move)
    }

    "parse LEFT commands" in {
      val lines = Iterator("LEFT")
      CommandParsing.toRobotCommands(lines).toList must be equalTo List(Left)
    }

    "parse RIGHT commands" in {
      val lines = Iterator("RIGHT")
      CommandParsing.toRobotCommands(lines).toList must be equalTo List(Right)
    }

    "parse REPORT commands" in {
      val lines = Iterator("REPORT")
      CommandParsing.toRobotCommands(lines).toList must be equalTo List(Report)
    }

    "with a DIAGONAL command" should {
      "parse valid commands" in {
        val lines = Iterator("DIAGONAL NORTH,WEST")
        CommandParsing.toRobotCommands(lines).toList must be equalTo List(Diagonal(North, West))
      }

      "not parse wrongly ordered diagonals" in {
        val lines = Iterator(
          "DIAGONAL WEST,NORTH"
        )
        CommandParsing.toRobotCommands(lines).toList must be equalTo List()
      }

      "not parse invalid first direction values" in {
        val lines = Iterator(
          "DIAGONAL NORTHSIDE,WEST",
          "DIAGONAL north,WEST",
          "DIAGONAL North,WEST",
          "DIAGONAL NORTHS,WEST"
        )
        CommandParsing.toRobotCommands(lines).toList must be equalTo List()
      }

      "not parse invalid second direction values" in {
        val lines = Iterator(
          "DIAGONAL NORTH,WESTSIDE",
          "DIAGONAL NORTH,west",
          "DIAGONAL NORTH,West",
          "DIAGONAL NORTH,WESTS"
        )
        CommandParsing.toRobotCommands(lines).toList must be equalTo List()
      }
    }

    "parse PLACE_OBJECT commands" in {
      val lines = Iterator("PLACE_OBJECT")
      CommandParsing.toRobotCommands(lines).toList must be equalTo List(PlaceObject)
    }

    "parse MAP commands" in {
      val lines = Iterator("MAP")
      CommandParsing.toRobotCommands(lines).toList must be equalTo List(MapCommand)
    }

    "not parse mystery commands" in {
      val lines = Iterator(
        "MOVER",
        "SHAKER",
        "LEFT FOR DEAD",
        "CLEFT",
        "BRIGHT",
        "REPORTING FOR DUTY"
      )
      CommandParsing.toRobotCommands(lines).toList must be equalTo List()
    }
  }
}
