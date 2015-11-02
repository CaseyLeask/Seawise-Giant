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
