package CommandExecution

import InputParsing._

class RunRobotScriptTest extends org.specs2.mutable.Specification {
  "RunRobotScript" should {
    "with an invalid state" should {
      "not move" in {
        RunRobotScript.nextState(InvalidState, Move) should beEqualTo(InvalidState)
      }

      "not turn left" in {
        RunRobotScript.nextState(InvalidState, Left) should beEqualTo(InvalidState)
      }

      "not turn right" in {
        RunRobotScript.nextState(InvalidState, Right) should beEqualTo(InvalidState)
      }

      "not report" in {
        RunRobotScript.nextState(InvalidState, Report) should beEqualTo(InvalidState)
      }

      "not go diagonal" in {
        RunRobotScript.nextState(InvalidState, Diagonal(North, West)) should beEqualTo(InvalidState)
      }

      "should place correctly" in {
        RunRobotScript.nextState(InvalidState, Place(0, 0, North)) should beEqualTo(ValidState(0, 0, North))
      }
    }

    "with a valid state" should {
      "when facing North" should {
        "not move when at the edge" in {
          RunRobotScript.nextState(ValidState(0, 4, North), Move) should beEqualTo(ValidState(0, 4, North))
        }

        "move when not at an edge" in {
          RunRobotScript.nextState(ValidState(0, 0, North), Move) should beEqualTo(ValidState(0, 1, North))
        }

        "turn left correctly" in {
          RunRobotScript.nextState(ValidState(0, 0, North), Left) should beEqualTo(ValidState(0, 0, West))
        }

        "turn right correctly" in {
          RunRobotScript.nextState(ValidState(0, 0, North), Right) should beEqualTo(ValidState(0, 0, East))
        }

        "go diagonally north east when not at an edge" in {
          RunRobotScript.nextState(ValidState(0, 0, North), Diagonal(North, East)) should beEqualTo(ValidState(1, 1, North))
        }

        "not go diagonally north east when at an edge" in {
          RunRobotScript.nextState(ValidState(4, 4, North), Diagonal(North, East)) should beEqualTo(ValidState(4, 4, North))
          RunRobotScript.nextState(ValidState(3, 4, North), Diagonal(North, East)) should beEqualTo(ValidState(3, 4, North))
          RunRobotScript.nextState(ValidState(4, 3, North), Diagonal(North, East)) should beEqualTo(ValidState(4, 3, North))
        }

        "not go diagonally south east" in {
          RunRobotScript.nextState(ValidState(0, 1, North), Diagonal(South, East)) should beEqualTo(ValidState(0, 1, North))
        }

        "not go diagonally south west" in {
          RunRobotScript.nextState(ValidState(1, 1, North), Diagonal(South, West)) should beEqualTo(ValidState(1, 1, North))
        }

        "go diagonally north west when not at an edge" in {
          RunRobotScript.nextState(ValidState(1, 0, North), Diagonal(North, West)) should beEqualTo(ValidState(0, 1, North))
        }

        "not go diagonally north west when at an edge" in {
          RunRobotScript.nextState(ValidState(0, 4, North), Diagonal(North, West)) should beEqualTo(ValidState(0, 4, North))
          RunRobotScript.nextState(ValidState(0, 3, North), Diagonal(North, West)) should beEqualTo(ValidState(0, 3, North))
          RunRobotScript.nextState(ValidState(1, 4, North), Diagonal(North, West)) should beEqualTo(ValidState(1, 4, North))
        }
      }

      "when facing East" should {
        "not move when at the edge" in {
          RunRobotScript.nextState(ValidState(4, 0, East), Move) should beEqualTo(ValidState(4, 0, East))
        }

        "move when not at an edge" in {
          RunRobotScript.nextState(ValidState(0, 0, East), Move) should beEqualTo(ValidState(1, 0, East))
        }

        "turn left correctly" in {
          RunRobotScript.nextState(ValidState(0, 0, East), Left) should beEqualTo(ValidState(0, 0, North))
        }

        "turn right correctly" in {
          RunRobotScript.nextState(ValidState(0, 0, East), Right) should beEqualTo(ValidState(0, 0, South))
        }

        "go diagonally north east when not at an edge" in {
          RunRobotScript.nextState(ValidState(0, 0, East), Diagonal(North, East)) should beEqualTo(ValidState(1, 1, East))
        }

        "not go diagonally north east when at an edge" in {
          RunRobotScript.nextState(ValidState(4, 4, East), Diagonal(North, East)) should beEqualTo(ValidState(4, 4, East))
          RunRobotScript.nextState(ValidState(3, 4, East), Diagonal(North, East)) should beEqualTo(ValidState(3, 4, East))
          RunRobotScript.nextState(ValidState(4, 3, East), Diagonal(North, East)) should beEqualTo(ValidState(4, 3, East))
        }

        "go diagonally south east when not at an edge" in {
          RunRobotScript.nextState(ValidState(0, 1, East), Diagonal(South, East)) should beEqualTo(ValidState(1, 0, East))
        }

        "not go diagonally south east when at an edge" in {
          RunRobotScript.nextState(ValidState(4, 0, East), Diagonal(South, East)) should beEqualTo(ValidState(4, 0, East))
          RunRobotScript.nextState(ValidState(4, 1, East), Diagonal(South, East)) should beEqualTo(ValidState(4, 1, East))
          RunRobotScript.nextState(ValidState(3, 0, East), Diagonal(South, East)) should beEqualTo(ValidState(3, 0, East))
        }

        "not go diagonally south west" in {
          RunRobotScript.nextState(ValidState(1, 1, East), Diagonal(South, West)) should beEqualTo(ValidState(1, 1, East))
        }

        "not go diagonally north west" in {
          RunRobotScript.nextState(ValidState(1, 0, East), Diagonal(North, West)) should beEqualTo(ValidState(1, 0, East))
        }
      }

      "when facing South" should {
        "not move when at the edge" in {
          RunRobotScript.nextState(ValidState(0, 0, South), Move) should beEqualTo(ValidState(0, 0, South))
        }

        "move when not at an edge" in {
          RunRobotScript.nextState(ValidState(0, 1, South), Move) should beEqualTo(ValidState(0, 0, South))
        }

        "turn left correctly" in {
          RunRobotScript.nextState(ValidState(0, 0, South), Left) should beEqualTo(ValidState(0, 0, East))
        }

        "turn right correctly" in {
          RunRobotScript.nextState(ValidState(0, 0, South), Right) should beEqualTo(ValidState(0, 0, West))
        }

        "not go diagonally north east" in {
          RunRobotScript.nextState(ValidState(0, 0, South), Diagonal(North, East)) should beEqualTo(ValidState(0, 0, South))
        }

        "go diagonally south east when not at an edge" in {
          RunRobotScript.nextState(ValidState(0, 1, South), Diagonal(South, East)) should beEqualTo(ValidState(1, 0, South))
        }

        "not go diagonally south east when at an edge" in {
          RunRobotScript.nextState(ValidState(4, 0, South), Diagonal(South, East)) should beEqualTo(ValidState(4, 0, South))
          RunRobotScript.nextState(ValidState(4, 1, South), Diagonal(South, East)) should beEqualTo(ValidState(4, 1, South))
          RunRobotScript.nextState(ValidState(3, 0, South), Diagonal(South, East)) should beEqualTo(ValidState(3, 0, South))
        }

        "go diagonally south west when not at an edge" in {
          RunRobotScript.nextState(ValidState(1, 1, South), Diagonal(South, West)) should beEqualTo(ValidState(0, 0, South))
        }

        "not go diagonally south west when at an edge" in {
          RunRobotScript.nextState(ValidState(0, 0, South), Diagonal(South, West)) should beEqualTo(ValidState(0, 0, South))
          RunRobotScript.nextState(ValidState(1, 0, South), Diagonal(South, West)) should beEqualTo(ValidState(1, 0, South))
          RunRobotScript.nextState(ValidState(0, 1, South), Diagonal(South, West)) should beEqualTo(ValidState(0, 1, South))
        }

        "not go diagonally north west" in {
          RunRobotScript.nextState(ValidState(1, 0, South), Diagonal(North, West)) should beEqualTo(ValidState(1, 0, South))
        }
      }

      "when facing West" should {
        "not move when at the edge" in {
          RunRobotScript.nextState(ValidState(0, 0, West), Move) should beEqualTo(ValidState(0, 0, West))
        }

        "move when not at an edge" in {
          RunRobotScript.nextState(ValidState(1, 0, West), Move) should beEqualTo(ValidState(0, 0, West))
        }

        "turn left correctly" in {
          RunRobotScript.nextState(ValidState(0, 0, West), Left) should beEqualTo(ValidState(0, 0, South))
        }

        "turn right correctly" in {
          RunRobotScript.nextState(ValidState(0, 0, West), Right) should beEqualTo(ValidState(0, 0, North))
        }

        "not go diagonally north east" in {
          RunRobotScript.nextState(ValidState(0, 0, West), Diagonal(North, East)) should beEqualTo(ValidState(0, 0, West))
        }

        "not go diagonally south east when not at an edge" in {
          RunRobotScript.nextState(ValidState(0, 1, West), Diagonal(South, East)) should beEqualTo(ValidState(0, 1, West))
        }

        "go diagonally south west when not at an edge" in {
          RunRobotScript.nextState(ValidState(1, 1, West), Diagonal(South, West)) should beEqualTo(ValidState(0, 0, West))
        }

        "not go diagonally south west when at an edge" in {
          RunRobotScript.nextState(ValidState(0, 0, West), Diagonal(South, West)) should beEqualTo(ValidState(0, 0, West))
          RunRobotScript.nextState(ValidState(1, 0, West), Diagonal(South, West)) should beEqualTo(ValidState(1, 0, West))
          RunRobotScript.nextState(ValidState(0, 1, West), Diagonal(South, West)) should beEqualTo(ValidState(0, 1, West))
        }

        "go diagonally north west when not at an edge" in {
          RunRobotScript.nextState(ValidState(1, 0, West), Diagonal(North, West)) should beEqualTo(ValidState(0, 1, West))
        }

        "not go diagonally north west when at an edge" in {
          RunRobotScript.nextState(ValidState(0, 4, West), Diagonal(North, West)) should beEqualTo(ValidState(0, 4, West))
          RunRobotScript.nextState(ValidState(1, 4, West), Diagonal(North, West)) should beEqualTo(ValidState(1, 4, West))
          RunRobotScript.nextState(ValidState(0, 3, West), Diagonal(North, West)) should beEqualTo(ValidState(0, 3, West))
        }
      }

      "report" in {
        RunRobotScript.nextState(ValidState(0, 0, North), Report) should beEqualTo(ValidState(0, 0, North))
      }

      "place correctly" in {
        RunRobotScript.nextState(ValidState(4, 4, North), Place(0, 0, North)) should beEqualTo(ValidState(0, 0, North))
      }
    }
  }
}