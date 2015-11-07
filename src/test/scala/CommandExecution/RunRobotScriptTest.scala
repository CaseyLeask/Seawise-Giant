package CommandExecution

import InputParsing._

class RunRobotScriptTest extends org.specs2.mutable.Specification {
  "RunRobotScript" should {
    "with an invalid state" should {
      "not move" in {
        RunRobotScript.nextResult(InvalidState, Move) should beEqualTo(RobotResult(InvalidState,()))
      }

      "not turn left" in {
        RunRobotScript.nextResult(InvalidState, Left) should beEqualTo(RobotResult(InvalidState,()))
      }

      "not turn right" in {
        RunRobotScript.nextResult(InvalidState, Right) should beEqualTo(RobotResult(InvalidState,()))
      }

      "not report" in {
        RunRobotScript.nextResult(InvalidState, Report) should beEqualTo(RobotResult(InvalidState,()))
      }

      "not go diagonal" in {
        RunRobotScript.nextResult(InvalidState, Diagonal(North, West)) should beEqualTo(RobotResult(InvalidState,()))
      }

      "should place correctly" in {
        RunRobotScript.nextResult(InvalidState, Place(0, 0, North)) should beEqualTo(RobotResult(ValidState(0, 0, North, Set()),()))
      }

      "should not place objects" in {
        RunRobotScript.nextResult(InvalidState, PlaceObject) should beEqualTo(RobotResult(InvalidState,()))
      }

      "should print a map correctly" in {
        RunRobotScript.nextResult(InvalidState, MapCommand) should beEqualTo(RobotResult(InvalidState,()))
        RunRobotScript.convertPlacedObjectsToMap(Set()) must beEqualTo("00000\n00000\n00000\n00000\n00000")
      }
    }

    "with a valid state" should {
      "when facing North" should {
        "not move when at the edge" in {
          RunRobotScript.nextResult(ValidState(0, 4, North, Set()), Move) should beEqualTo(RobotResult(ValidState(0, 4, North, Set()),()))
        }

        "move when not at an edge" in {
          RunRobotScript.nextResult(ValidState(0, 0, North, Set()), Move) should beEqualTo(RobotResult(ValidState(0, 1, North, Set()),()))
        }

        "turn left correctly" in {
          RunRobotScript.nextResult(ValidState(0, 0, North, Set()), Left) should beEqualTo(RobotResult(ValidState(0, 0, West, Set()),()))
        }

        "turn right correctly" in {
          RunRobotScript.nextResult(ValidState(0, 0, North, Set()), Right) should beEqualTo(RobotResult(ValidState(0, 0, East, Set()),()))
        }

        "go diagonally north east when not at an edge" in {
          RunRobotScript.nextResult(ValidState(0, 0, North, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(1, 1, North, Set()),()))
        }

        "not go diagonally north east when at an edge" in {
          RunRobotScript.nextResult(ValidState(4, 4, North, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(4, 4, North, Set()),()))
          RunRobotScript.nextResult(ValidState(3, 4, North, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(3, 4, North, Set()),()))
          RunRobotScript.nextResult(ValidState(4, 3, North, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(4, 3, North, Set()),()))
        }

        "not go diagonally south east" in {
          RunRobotScript.nextResult(ValidState(0, 1, North, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(0, 1, North, Set()),()))
        }

        "not go diagonally south west" in {
          RunRobotScript.nextResult(ValidState(1, 1, North, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(1, 1, North, Set()),()))
        }

        "go diagonally north west when not at an edge" in {
          RunRobotScript.nextResult(ValidState(1, 0, North, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(0, 1, North, Set()),()))
        }

        "not go diagonally north west when at an edge" in {
          RunRobotScript.nextResult(ValidState(0, 4, North, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(0, 4, North, Set()),()))
          RunRobotScript.nextResult(ValidState(0, 3, North, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(0, 3, North, Set()),()))
          RunRobotScript.nextResult(ValidState(1, 4, North, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(1, 4, North, Set()),()))
        }

        "place objects correctly" in {
          RunRobotScript.nextResult(ValidState(0, 0, North, Set()), PlaceObject) should beEqualTo(RobotResult(ValidState(0, 0, North, Set(PlacedObject(0, 1))),()))
        }

        "not move when an object is in front of the robot" in {
          RunRobotScript.nextResult(ValidState(0, 0, North, Set(PlacedObject(0, 1))), Move) should
            beEqualTo(RobotResult(ValidState(0, 0, North, Set(PlacedObject(0, 1))),()))
        }
      }

      "when facing East" should {
        "not move when at the edge" in {
          RunRobotScript.nextResult(ValidState(4, 0, East, Set()), Move) should beEqualTo(RobotResult(ValidState(4, 0, East, Set()),()))
        }

        "move when not at an edge" in {
          RunRobotScript.nextResult(ValidState(0, 0, East, Set()), Move) should beEqualTo(RobotResult(ValidState(1, 0, East, Set()),()))
        }

        "turn left correctly" in {
          RunRobotScript.nextResult(ValidState(0, 0, East, Set()), Left) should beEqualTo(RobotResult(ValidState(0, 0, North, Set()),()))
        }

        "turn right correctly" in {
          RunRobotScript.nextResult(ValidState(0, 0, East, Set()), Right) should beEqualTo(RobotResult(ValidState(0, 0, South, Set()),()))
        }

        "go diagonally north east when not at an edge" in {
          RunRobotScript.nextResult(ValidState(0, 0, East, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(1, 1, East, Set()),()))
        }

        "not go diagonally north east when at an edge" in {
          RunRobotScript.nextResult(ValidState(4, 4, East, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(4, 4, East, Set()),()))
          RunRobotScript.nextResult(ValidState(3, 4, East, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(3, 4, East, Set()),()))
          RunRobotScript.nextResult(ValidState(4, 3, East, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(4, 3, East, Set()),()))
        }

        "go diagonally south east when not at an edge" in {
          RunRobotScript.nextResult(ValidState(0, 1, East, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(1, 0, East, Set()),()))
        }

        "not go diagonally south east when at an edge" in {
          RunRobotScript.nextResult(ValidState(4, 0, East, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(4, 0, East, Set()),()))
          RunRobotScript.nextResult(ValidState(4, 1, East, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(4, 1, East, Set()),()))
          RunRobotScript.nextResult(ValidState(3, 0, East, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(3, 0, East, Set()),()))
        }

        "not go diagonally south west" in {
          RunRobotScript.nextResult(ValidState(1, 1, East, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(1, 1, East, Set()),()))
        }

        "not go diagonally north west" in {
          RunRobotScript.nextResult(ValidState(1, 0, East, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(1, 0, East, Set()),()))
        }

        "place objects correctly" in {
          RunRobotScript.nextResult(ValidState(0, 0, East, Set()), PlaceObject) should beEqualTo(RobotResult(ValidState(0, 0, East, Set(PlacedObject(1, 0))),()))
        }

        "not move when an object is in front of the robot" in {
          RunRobotScript.nextResult(ValidState(0, 0, East, Set(PlacedObject(1, 0))), Move) should
            beEqualTo(RobotResult(ValidState(0, 0, East, Set(PlacedObject(1, 0))),()))
        }
      }

      "when facing South" should {
        "not move when at the edge" in {
          RunRobotScript.nextResult(ValidState(0, 0, South, Set()), Move) should beEqualTo(RobotResult(ValidState(0, 0, South, Set()),()))
        }

        "move when not at an edge" in {
          RunRobotScript.nextResult(ValidState(0, 1, South, Set()), Move) should beEqualTo(RobotResult(ValidState(0, 0, South, Set()),()))
        }

        "turn left correctly" in {
          RunRobotScript.nextResult(ValidState(0, 0, South, Set()), Left) should beEqualTo(RobotResult(ValidState(0, 0, East, Set()),()))
        }

        "turn right correctly" in {
          RunRobotScript.nextResult(ValidState(0, 0, South, Set()), Right) should beEqualTo(RobotResult(ValidState(0, 0, West, Set()),()))
        }

        "not go diagonally north east" in {
          RunRobotScript.nextResult(ValidState(0, 0, South, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(0, 0, South, Set()),()))
        }

        "go diagonally south east when not at an edge" in {
          RunRobotScript.nextResult(ValidState(0, 1, South, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(1, 0, South, Set()),()))
        }

        "not go diagonally south east when at an edge" in {
          RunRobotScript.nextResult(ValidState(4, 0, South, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(4, 0, South, Set()),()))
          RunRobotScript.nextResult(ValidState(4, 1, South, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(4, 1, South, Set()),()))
          RunRobotScript.nextResult(ValidState(3, 0, South, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(3, 0, South, Set()),()))
        }

        "go diagonally south west when not at an edge" in {
          RunRobotScript.nextResult(ValidState(1, 1, South, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(0, 0, South, Set()),()))
        }

        "not go diagonally south west when at an edge" in {
          RunRobotScript.nextResult(ValidState(0, 0, South, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(0, 0, South, Set()),()))
          RunRobotScript.nextResult(ValidState(1, 0, South, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(1, 0, South, Set()),()))
          RunRobotScript.nextResult(ValidState(0, 1, South, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(0, 1, South, Set()),()))
        }

        "not go diagonally north west" in {
          RunRobotScript.nextResult(ValidState(1, 0, South, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(1, 0, South, Set()),()))
        }

        "place objects correctly" in {
          RunRobotScript.nextResult(ValidState(0, 1, South, Set()), PlaceObject) should beEqualTo(RobotResult(ValidState(0, 1, South, Set(PlacedObject(0, 0))),()))
        }

        "not move when an object is in front of the robot" in {
          RunRobotScript.nextResult(ValidState(0, 1, South, Set(PlacedObject(0, 0))), Move) should
            beEqualTo(RobotResult(ValidState(0, 1, South, Set(PlacedObject(0, 0))),()))
        }
      }

      "when facing West" should {
        "not move when at the edge" in {
          RunRobotScript.nextResult(ValidState(0, 0, West, Set()), Move) should beEqualTo(RobotResult(ValidState(0, 0, West, Set()),()))
        }

        "move when not at an edge" in {
          RunRobotScript.nextResult(ValidState(1, 0, West, Set()), Move) should beEqualTo(RobotResult(ValidState(0, 0, West, Set()),()))
        }

        "turn left correctly" in {
          RunRobotScript.nextResult(ValidState(0, 0, West, Set()), Left) should beEqualTo(RobotResult(ValidState(0, 0, South, Set()),()))
        }

        "turn right correctly" in {
          RunRobotScript.nextResult(ValidState(0, 0, West, Set()), Right) should beEqualTo(RobotResult(ValidState(0, 0, North, Set()),()))
        }

        "not go diagonally north east" in {
          RunRobotScript.nextResult(ValidState(0, 0, West, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(0, 0, West, Set()),()))
        }

        "not go diagonally south east when not at an edge" in {
          RunRobotScript.nextResult(ValidState(0, 1, West, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(0, 1, West, Set()),()))
        }

        "go diagonally south west when not at an edge" in {
          RunRobotScript.nextResult(ValidState(1, 1, West, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(0, 0, West, Set()),()))
        }

        "not go diagonally south west when at an edge" in {
          RunRobotScript.nextResult(ValidState(0, 0, West, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(0, 0, West, Set()),()))
          RunRobotScript.nextResult(ValidState(1, 0, West, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(1, 0, West, Set()),()))
          RunRobotScript.nextResult(ValidState(0, 1, West, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(0, 1, West, Set()),()))
        }

        "go diagonally north west when not at an edge" in {
          RunRobotScript.nextResult(ValidState(1, 0, West, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(0, 1, West, Set()),()))
        }

        "not go diagonally north west when at an edge" in {
          RunRobotScript.nextResult(ValidState(0, 4, West, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(0, 4, West, Set()),()))
          RunRobotScript.nextResult(ValidState(1, 4, West, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(1, 4, West, Set()),()))
          RunRobotScript.nextResult(ValidState(0, 3, West, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(0, 3, West, Set()),()))
        }

        "place objects correctly" in {
          RunRobotScript.nextResult(ValidState(1, 0, West, Set()), PlaceObject) should beEqualTo(RobotResult(ValidState(1, 0, West, Set(PlacedObject(0, 0))),()))
        }

        "not move when an object is in front of the robot" in {
          RunRobotScript.nextResult(ValidState(1, 0, West, Set(PlacedObject(0, 0))), Move) should
            beEqualTo(RobotResult(ValidState(1, 0, West, Set(PlacedObject(0, 0))),()))
        }
      }

      "report" in {
        RunRobotScript.nextResult(ValidState(0, 0, North, Set()), Report) should beEqualTo(RobotResult(ValidState(0, 0, North, Set()),"0,0,NORTH"))
      }

      "place correctly" in {
        RunRobotScript.nextResult(ValidState(4, 4, North, Set()), Place(0, 0, North)) should beEqualTo(RobotResult(ValidState(0, 0, North, Set()),()))
      }

      "place objects correctly when there are existing objects" in {
        RunRobotScript.nextResult(ValidState(0, 0, North, Set(PlacedObject(1, 0))), PlaceObject) should
          beEqualTo(RobotResult(ValidState(0, 0, North, Set(PlacedObject(1, 0), PlacedObject(0, 1))),()))
      }

      "move when an object is not in front of the robot" in {
        RunRobotScript.nextResult(ValidState(0, 0, North, Set(PlacedObject(1, 0))), Move) should
          beEqualTo(RobotResult(ValidState(0, 1, North, Set(PlacedObject(1, 0))),()))
      }

      "when objects are placed diagonally from the robot" should {
        "not move north east" in {
          RunRobotScript.nextResult(ValidState(0, 0, North, Set(PlacedObject(1, 1))), Diagonal(North, East)) should
            beEqualTo(RobotResult(ValidState(0, 0, North, Set(PlacedObject(1, 1))),()))
        }

        "not move south east" in {
          RunRobotScript.nextResult(ValidState(0, 1, East, Set(PlacedObject(1, 0))), Diagonal(South, East)) should
            beEqualTo(RobotResult(ValidState(0, 1, East, Set(PlacedObject(1, 0))),()))
        }

        "not move south west" in {
          RunRobotScript.nextResult(ValidState(1, 1, South, Set(PlacedObject(0, 0))), Diagonal(South, West)) should
            beEqualTo(RobotResult(ValidState(1, 1, South, Set(PlacedObject(0, 0))),()))
        }

        "not move north west" in {
          RunRobotScript.nextResult(ValidState(1, 0, West, Set(PlacedObject(0, 1))), Diagonal(North, West)) should
            beEqualTo(RobotResult(ValidState(1, 0, West, Set(PlacedObject(0, 1))),()))
        }
      }

      "when there are no objects placed" should {
        "should print a map correctly" in {
          val state = ValidState(0, 0, North, Set())
          RunRobotScript.nextResult(state, MapCommand) should beEqualTo(RobotResult(state, "00000\n00000\n00000\n00000\n00000"))
        }
      }

      "when there are objects placed" should {
        "should print a map correctly"  in {
          val state = ValidState(0, 0, North, Set(PlacedObject(1, 1)))
          RunRobotScript.nextResult(state, MapCommand) should beEqualTo(RobotResult(state, "00000\n00000\n00000\n0X000\n00000"))
        }
      }
    }
  }
}
