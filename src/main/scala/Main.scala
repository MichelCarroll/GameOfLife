
import org.scalajs.dom.{document, CanvasRenderingContext2D}
import org.scalajs.dom.html.Canvas

import scala.scalajs.js.timers._

case class Cell(x: Int, y: Int) {
  lazy val neighbors: Set[Cell] = (
    for {
      x0 <- (x - 1) to (x + 1)
      y0 <- (y - 1) to (y + 1)
    } yield Cell(x0, y0)
  ).toSet - this
}

case class Time(value: Int) extends AnyVal {
  def next = Time(value + 1)
}

object Time {
  val initial = Time(0)
}

case class Map(width: Int, height: Int, initiallyActiveCells: Set[Cell]) {

  val cells: Set[Cell] = (
    for {
      x0 <- 1 to width
      y0 <- 1 to height
    } yield Cell(x0, y0)
  ).toSet

  val frames: Stream[Frame] = Stream.iterate(Frame(Time.initial, initiallyActiveCells))(lastFrame =>
    Frame(
      lastFrame.time.next,
      cells.filter(cell => {
        val pastLiveNeighborCount = cell.neighbors.intersect(lastFrame.activeCellPositions).size
        val wasAlive = lastFrame.activeCellPositions.contains(cell)

        (pastLiveNeighborCount, wasAlive) match {
          case (2, true) => true
          case (3, true) => true
          case (_, true) => false
          case (3, false) => true
          case _ => false
        }
      })
    )
  )
}

case class Frame(time: Time, activeCellPositions: Set[Cell])

object Main {

  def main(args: Array[String]): Unit = {

    val map = Map(
      width = 60,
      height = 60,
      Set(
        Cell(10, 10),
        Cell(11, 11),
        Cell(12, 11),
        Cell(11, 12),
        Cell(10, 12),
      )
    )

    var frames = map.frames

    val mainCanvas = document.createElement("canvas").asInstanceOf[Canvas]
    val cellSize = 5
    mainCanvas.width = map.width * cellSize
    mainCanvas.height = map.height * cellSize
    document.body.appendChild(mainCanvas)
    val ctx = mainCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    def draw(frame: Frame): Unit = {

      def drawBackground(): Unit = {
        ctx.fillStyle = "black"
        ctx.fillRect(0, 0, mainCanvas.width, mainCanvas.height)
      }

      def drawCells(): Unit = {
        ctx.fillStyle = "white"
        frame.activeCellPositions
          .foreach {
            case Cell(x, y)  =>
              ctx.fillRect(
                (x - 1) * cellSize,
                (y - 1) * cellSize,
                cellSize,
                cellSize
              )
          }
      }

      drawBackground()
      drawCells()
    }


    draw(frames.head)
    setInterval(100) {
      frames = frames.tail
      draw(frames.head)
    }

  }

}
