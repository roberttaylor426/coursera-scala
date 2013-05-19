package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) {
        case (block, move) => move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }
  
  trait UnsolvableLevel extends SolutionChecker {
    val level = "ST"
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(4, 1)), "4,1")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(6, 0)), "6,0")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
      assert(goal == Pos(4, 7))
    }
  }

  test("block is standing") {
    new Level1 {
      assert(Block(Pos(0, 0), new Pos(0, 0)).isStanding)
      assert(!Block(Pos(0, 0), new Pos(0, 1)).isStanding)
    }
  }

  test("block is legal") {
    new Level1 {
      assert(Block(Pos(0, 0), new Pos(0, 1)).isLegal)
      assert(!Block(Pos(4, 0), new Pos(4, 1)).isLegal)
    }
  }

  test("starting block position") {
    new Level1 {
      assert(startBlock.isStanding)
      assert(startBlock.b1.x == 1)
      assert(startBlock.b2.x == 1)
    }
  }

  test("legal neighbors") {
    new Level1 {
      val blockAtTopLeft = Block(Pos(0, 0), Pos(0, 0))
      assert(blockAtTopLeft.legalNeighbors.length == 2)
      assert(blockAtTopLeft.legalNeighbors.contains((Block(Pos(1, 0), Pos(2, 0)), Down)))
      assert(blockAtTopLeft.legalNeighbors.contains((Block(Pos(0, 1), Pos(0, 2)), Right)))
    }
  }

  test("done!") {
    new Level1 {
      assert(done(Block(Pos(4, 7), Pos(4, 7))))
      assert(!done(Block(Pos(0, 7), Pos(0, 7))))
    }
  }

  test("neighbours with history") {
    new Level1 {
      val neighboursWithHistory = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up))
      assert(neighboursWithHistory.toSet ===
        Set((Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))))
    }
  }

  test("neighbours with history avoiding circles") {
    new Level1 {
      val newNeighbours = newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream, 
          Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1))))

      assert(newNeighbours ===
        Set((Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
  
  test("empty list should be returned when no solution available") {
    new UnsolvableLevel {
      assert(solution.length === 0)
    }
  }
}
