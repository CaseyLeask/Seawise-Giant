package State

import Commands._
import Direction.{East, North, South, West}
import State._

class RobotStateTest extends org.specs2.mutable.Specification {
  "RobotState" should {
    "with an invalid state" should {
      "not move" in {
        RobotState.nextResult(InvalidState, Move) should beEqualTo(RobotResult(InvalidState,None))
      }

      "not turn left" in {
        RobotState.nextResult(InvalidState, Left) should beEqualTo(RobotResult(InvalidState,None))
      }

      "not turn right" in {
        RobotState.nextResult(InvalidState, Right) should beEqualTo(RobotResult(InvalidState,None))
      }

      "not report" in {
        RobotState.nextResult(InvalidState, Report) should beEqualTo(RobotResult(InvalidState,None))
      }

      "not go diagonal" in {
        RobotState.nextResult(InvalidState, Diagonal(North, West)) should beEqualTo(RobotResult(InvalidState,None))
      }

      "place in a valid location" in {
        RobotState.nextResult(InvalidState, Place(0, 0, North)) should beEqualTo(RobotResult(ValidState(0, 0, North, Set()),None))
      }

      "not place in an invalid location" in {
        RobotState.nextResult(InvalidState, Place(6, 0, North)) should beEqualTo(RobotResult(InvalidState))
        RobotState.nextResult(InvalidState, Place(0, 6, North)) should beEqualTo(RobotResult(InvalidState))
      }

      "should not place objects" in {
        RobotState.nextResult(InvalidState, PlaceObject) should beEqualTo(RobotResult(InvalidState,None))
      }

      "should print a map correctly" in {
        RobotState.nextResult(InvalidState, MapCommand) should beEqualTo(RobotResult(InvalidState,None))
      }
    }

    "with a valid state" should {
      "when facing North" should {
        "not move when at the edge" in {
          RobotState.nextResult(ValidState(0, 4, North, Set()), Move) should beEqualTo(RobotResult(ValidState(0, 4, North, Set()),None))
        }

        "move when not at an edge" in {
          RobotState.nextResult(ValidState(0, 0, North, Set()), Move) should beEqualTo(RobotResult(ValidState(0, 1, North, Set()),None))
        }

        "turn left correctly" in {
          RobotState.nextResult(ValidState(0, 0, North, Set()), Left) should beEqualTo(RobotResult(ValidState(0, 0, West, Set()),None))
        }

        "turn right correctly" in {
          RobotState.nextResult(ValidState(0, 0, North, Set()), Right) should beEqualTo(RobotResult(ValidState(0, 0, East, Set()),None))
        }

        "go diagonally north east when not at an edge" in {
          RobotState.nextResult(ValidState(0, 0, North, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(1, 1, North, Set()),None))
        }

        "not go diagonally north east when at an edge" in {
          RobotState.nextResult(ValidState(4, 4, North, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(4, 4, North, Set()),None))
          RobotState.nextResult(ValidState(3, 4, North, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(3, 4, North, Set()),None))
          RobotState.nextResult(ValidState(4, 3, North, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(4, 3, North, Set()),None))
        }

        "not go diagonally south east" in {
          RobotState.nextResult(ValidState(0, 1, North, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(0, 1, North, Set()),None))
        }

        "not go diagonally south west" in {
          RobotState.nextResult(ValidState(1, 1, North, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(1, 1, North, Set()),None))
        }

        "go diagonally north west when not at an edge" in {
          RobotState.nextResult(ValidState(1, 0, North, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(0, 1, North, Set()),None))
        }

        "not go diagonally north west when at an edge" in {
          RobotState.nextResult(ValidState(0, 4, North, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(0, 4, North, Set()),None))
          RobotState.nextResult(ValidState(0, 3, North, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(0, 3, North, Set()),None))
          RobotState.nextResult(ValidState(1, 4, North, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(1, 4, North, Set()),None))
        }

        "place objects correctly" in {
          RobotState.nextResult(ValidState(0, 0, North, Set()), PlaceObject) should beEqualTo(RobotResult(ValidState(0, 0, North, Set(PlacedObject(0, 1))),None))
        }

        "not move when an object is in front of the robot" in {
          RobotState.nextResult(ValidState(0, 0, North, Set(PlacedObject(0, 1))), Move) should
            beEqualTo(RobotResult(ValidState(0, 0, North, Set(PlacedObject(0, 1))),None))
        }
      }

      "when facing East" should {
        "not move when at the edge" in {
          RobotState.nextResult(ValidState(4, 0, East, Set()), Move) should beEqualTo(RobotResult(ValidState(4, 0, East, Set()),None))
        }

        "move when not at an edge" in {
          RobotState.nextResult(ValidState(0, 0, East, Set()), Move) should beEqualTo(RobotResult(ValidState(1, 0, East, Set()),None))
        }

        "turn left correctly" in {
          RobotState.nextResult(ValidState(0, 0, East, Set()), Left) should beEqualTo(RobotResult(ValidState(0, 0, North, Set()),None))
        }

        "turn right correctly" in {
          RobotState.nextResult(ValidState(0, 0, East, Set()), Right) should beEqualTo(RobotResult(ValidState(0, 0, South, Set()),None))
        }

        "go diagonally north east when not at an edge" in {
          RobotState.nextResult(ValidState(0, 0, East, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(1, 1, East, Set()),None))
        }

        "not go diagonally north east when at an edge" in {
          RobotState.nextResult(ValidState(4, 4, East, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(4, 4, East, Set()),None))
          RobotState.nextResult(ValidState(3, 4, East, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(3, 4, East, Set()),None))
          RobotState.nextResult(ValidState(4, 3, East, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(4, 3, East, Set()),None))
        }

        "go diagonally south east when not at an edge" in {
          RobotState.nextResult(ValidState(0, 1, East, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(1, 0, East, Set()),None))
        }

        "not go diagonally south east when at an edge" in {
          RobotState.nextResult(ValidState(4, 0, East, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(4, 0, East, Set()),None))
          RobotState.nextResult(ValidState(4, 1, East, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(4, 1, East, Set()),None))
          RobotState.nextResult(ValidState(3, 0, East, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(3, 0, East, Set()),None))
        }

        "not go diagonally south west" in {
          RobotState.nextResult(ValidState(1, 1, East, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(1, 1, East, Set()),None))
        }

        "not go diagonally north west" in {
          RobotState.nextResult(ValidState(1, 0, East, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(1, 0, East, Set()),None))
        }

        "place objects correctly" in {
          RobotState.nextResult(ValidState(0, 0, East, Set()), PlaceObject) should beEqualTo(RobotResult(ValidState(0, 0, East, Set(PlacedObject(1, 0))),None))
        }

        "not move when an object is in front of the robot" in {
          RobotState.nextResult(ValidState(0, 0, East, Set(PlacedObject(1, 0))), Move) should
            beEqualTo(RobotResult(ValidState(0, 0, East, Set(PlacedObject(1, 0))),None))
        }
      }

      "when facing South" should {
        "not move when at the edge" in {
          RobotState.nextResult(ValidState(0, 0, South, Set()), Move) should beEqualTo(RobotResult(ValidState(0, 0, South, Set()),None))
        }

        "move when not at an edge" in {
          RobotState.nextResult(ValidState(0, 1, South, Set()), Move) should beEqualTo(RobotResult(ValidState(0, 0, South, Set()),None))
        }

        "turn left correctly" in {
          RobotState.nextResult(ValidState(0, 0, South, Set()), Left) should beEqualTo(RobotResult(ValidState(0, 0, East, Set()),None))
        }

        "turn right correctly" in {
          RobotState.nextResult(ValidState(0, 0, South, Set()), Right) should beEqualTo(RobotResult(ValidState(0, 0, West, Set()),None))
        }

        "not go diagonally north east" in {
          RobotState.nextResult(ValidState(0, 0, South, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(0, 0, South, Set()),None))
        }

        "go diagonally south east when not at an edge" in {
          RobotState.nextResult(ValidState(0, 1, South, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(1, 0, South, Set()),None))
        }

        "not go diagonally south east when at an edge" in {
          RobotState.nextResult(ValidState(4, 0, South, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(4, 0, South, Set()),None))
          RobotState.nextResult(ValidState(4, 1, South, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(4, 1, South, Set()),None))
          RobotState.nextResult(ValidState(3, 0, South, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(3, 0, South, Set()),None))
        }

        "go diagonally south west when not at an edge" in {
          RobotState.nextResult(ValidState(1, 1, South, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(0, 0, South, Set()),None))
        }

        "not go diagonally south west when at an edge" in {
          RobotState.nextResult(ValidState(0, 0, South, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(0, 0, South, Set()),None))
          RobotState.nextResult(ValidState(1, 0, South, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(1, 0, South, Set()),None))
          RobotState.nextResult(ValidState(0, 1, South, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(0, 1, South, Set()),None))
        }

        "not go diagonally north west" in {
          RobotState.nextResult(ValidState(1, 0, South, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(1, 0, South, Set()),None))
        }

        "place objects correctly" in {
          RobotState.nextResult(ValidState(0, 1, South, Set()), PlaceObject) should beEqualTo(RobotResult(ValidState(0, 1, South, Set(PlacedObject(0, 0))),None))
        }

        "not move when an object is in front of the robot" in {
          RobotState.nextResult(ValidState(0, 1, South, Set(PlacedObject(0, 0))), Move) should
            beEqualTo(RobotResult(ValidState(0, 1, South, Set(PlacedObject(0, 0))),None))
        }
      }

      "when facing West" should {
        "not move when at the edge" in {
          RobotState.nextResult(ValidState(0, 0, West, Set()), Move) should beEqualTo(RobotResult(ValidState(0, 0, West, Set()),None))
        }

        "move when not at an edge" in {
          RobotState.nextResult(ValidState(1, 0, West, Set()), Move) should beEqualTo(RobotResult(ValidState(0, 0, West, Set()),None))
        }

        "turn left correctly" in {
          RobotState.nextResult(ValidState(0, 0, West, Set()), Left) should beEqualTo(RobotResult(ValidState(0, 0, South, Set()),None))
        }

        "turn right correctly" in {
          RobotState.nextResult(ValidState(0, 0, West, Set()), Right) should beEqualTo(RobotResult(ValidState(0, 0, North, Set()),None))
        }

        "not go diagonally north east" in {
          RobotState.nextResult(ValidState(0, 0, West, Set()), Diagonal(North, East)) should beEqualTo(RobotResult(ValidState(0, 0, West, Set()),None))
        }

        "not go diagonally south east when not at an edge" in {
          RobotState.nextResult(ValidState(0, 1, West, Set()), Diagonal(South, East)) should beEqualTo(RobotResult(ValidState(0, 1, West, Set()),None))
        }

        "go diagonally south west when not at an edge" in {
          RobotState.nextResult(ValidState(1, 1, West, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(0, 0, West, Set()),None))
        }

        "not go diagonally south west when at an edge" in {
          RobotState.nextResult(ValidState(0, 0, West, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(0, 0, West, Set()),None))
          RobotState.nextResult(ValidState(1, 0, West, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(1, 0, West, Set()),None))
          RobotState.nextResult(ValidState(0, 1, West, Set()), Diagonal(South, West)) should beEqualTo(RobotResult(ValidState(0, 1, West, Set()),None))
        }

        "go diagonally north west when not at an edge" in {
          RobotState.nextResult(ValidState(1, 0, West, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(0, 1, West, Set()),None))
        }

        "not go diagonally north west when at an edge" in {
          RobotState.nextResult(ValidState(0, 4, West, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(0, 4, West, Set()),None))
          RobotState.nextResult(ValidState(1, 4, West, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(1, 4, West, Set()),None))
          RobotState.nextResult(ValidState(0, 3, West, Set()), Diagonal(North, West)) should beEqualTo(RobotResult(ValidState(0, 3, West, Set()),None))
        }

        "place objects correctly" in {
          RobotState.nextResult(ValidState(1, 0, West, Set()), PlaceObject) should beEqualTo(RobotResult(ValidState(1, 0, West, Set(PlacedObject(0, 0))),None))
        }

        "not move when an object is in front of the robot" in {
          RobotState.nextResult(ValidState(1, 0, West, Set(PlacedObject(0, 0))), Move) should
            beEqualTo(RobotResult(ValidState(1, 0, West, Set(PlacedObject(0, 0))),None))
        }
      }

      "report" in {
        RobotState.nextResult(ValidState(0, 0, North, Set()), Report) should beEqualTo(RobotResult(ValidState(0, 0, North, Set()),Some("0,0,NORTH")))
      }

      "place with a valid command" in {
        RobotState.nextResult(ValidState(4, 4, North, Set()), Place(0, 0, North)) should beEqualTo(RobotResult(ValidState(0, 0, North, Set()),None))
      }

      "not place with an invalid command" in {
        RobotState.nextResult(ValidState(4, 4, North, Set()), Place(6, 0, North)) should beEqualTo(RobotResult(ValidState(4, 4, North, Set()),None))
        RobotState.nextResult(ValidState(4, 4, North, Set()), Place(0, 6, North)) should beEqualTo(RobotResult(ValidState(4, 4, North, Set()),None))
      }

      "place objects correctly when there are existing objects" in {
        RobotState.nextResult(ValidState(0, 0, North, Set(PlacedObject(1, 0))), PlaceObject) should
          beEqualTo(RobotResult(ValidState(0, 0, North, Set(PlacedObject(1, 0), PlacedObject(0, 1))),None))
      }

      "move when an object is not in front of the robot" in {
        RobotState.nextResult(ValidState(0, 0, North, Set(PlacedObject(1, 0))), Move) should
          beEqualTo(RobotResult(ValidState(0, 1, North, Set(PlacedObject(1, 0))),None))
      }

      "when objects are placed diagonally from the robot" should {
        "not move north east" in {
          RobotState.nextResult(ValidState(0, 0, North, Set(PlacedObject(1, 1))), Diagonal(North, East)) should
            beEqualTo(RobotResult(ValidState(0, 0, North, Set(PlacedObject(1, 1))),None))
        }

        "not move south east" in {
          RobotState.nextResult(ValidState(0, 1, East, Set(PlacedObject(1, 0))), Diagonal(South, East)) should
            beEqualTo(RobotResult(ValidState(0, 1, East, Set(PlacedObject(1, 0))),None))
        }

        "not move south west" in {
          RobotState.nextResult(ValidState(1, 1, South, Set(PlacedObject(0, 0))), Diagonal(South, West)) should
            beEqualTo(RobotResult(ValidState(1, 1, South, Set(PlacedObject(0, 0))),None))
        }

        "not move north west" in {
          RobotState.nextResult(ValidState(1, 0, West, Set(PlacedObject(0, 1))), Diagonal(North, West)) should
            beEqualTo(RobotResult(ValidState(1, 0, West, Set(PlacedObject(0, 1))),None))
        }
      }

      "when there are no objects placed" should {
        "print a map correctly" in {
          val state = ValidState(0, 0, North, Set())
          RobotState.nextResult(state, MapCommand) should beEqualTo(RobotResult(state, Some("00000\n00000\n00000\n00000\n00000")))
        }
      }

      "when there are objects placed" should {
        "print a map correctly"  in {
          val state = ValidState(0, 0, North, Set(PlacedObject(1, 1)))
          RobotState.nextResult(state, MapCommand) should beEqualTo(RobotResult(state, Some("00000\n00000\n00000\n0X000\n00000")))
        }

        "not place robot on an object" in {
          val state = ValidState(0, 0, North, Set(PlacedObject(1, 1)))
          RobotState.nextResult(state, Place(1, 1, North)) should beEqualTo(RobotResult(state, None))
        }
      }
    }
  }
}
