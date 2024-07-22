import domain.{GTDUnit, GtdThought}

import java.text.SimpleDateFormat
import java.util.Date
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.sys.exit
import scala.util.{Failure, Random, Success, Try}


object Main {
  class CliReadingMechanism extends ReadingMechanism {
    override def askFreeQuestion(q: String): Future[String] = Future {
      println(q);
      scala.io.StdIn.readLine();
    }

    val format = new SimpleDateFormat("dd-MM-yyyy")

    override def askTimeQuestion(q: String): Future[Date] = Future {
      format.parse(ask_with_predicate(s => Try(format.parse(s)) match {
        case Failure(exception) => false
        case Success(value) => true
      }, q))
    }

    def ask_with_predicate(p: String => Boolean, question: String): String = {
      println(question)
      val ans = scala.io.StdIn.readLine()
      if (p(ans)) ans else ask_with_predicate(p, question)
    }

    override def askSelectiveQuestion(q: String, l: List[String]): Future[String] = Future {
      ask_with_predicate(l.contains(_), q + s"\n${l.mkString(" | ")}")
    }

    override def shout(message: String): Unit = println(message)
  }

  def putNew(meh: ReadingMechanism, unit: GTDUnit): Future[Unit] =
    meh.askFreeQuestion("your thought here:").map(a => unit.basket = unit.basket.appended(GtdThought(a)))

  def process(card: GtdThought, meh: ReadingMechanism, unit: GTDUnit): Future[Unit] = for {
    card <- CardConstructors.constructGenericCard(card, meh)
  } yield {
    unit.put(card) match {
      case Some(value) => println(value)
      case None => print("")
    }

  }

  def show(unit: GTDUnit): Future[Unit] = Future {
    println("BASKET", unit.basket)
    println("NOTES", unit.notes)
    println("TRASH", unit.trash)
    println("DELEGATED", unit.delegatedTasks)
    println("CALENDAR", unit.calendar)
    println("LATER", unit.later)
    println("NOW", unit.now)
    println("PROJECTS", unit.projects)
  }

  val random = new Random

  def run(unit: GTDUnit, meh: ReadingMechanism): Unit = {
    println("enter command: put, process, show,end")
    val command = scala.io.StdIn.readLine()
    val future = command match {
      case "put" => putNew(meh, unit)
      case "process" =>
        if (unit.basket.isEmpty) Future {
          println("Seems like you dont have cards in basket!")
        } else {
          val task = unit.basket(random.nextInt(unit.basket.length))
          unit.basket = unit.basket.filter(_ != task)
          process(task, meh, unit)
        }
      case "show" => {
        show(unit)
      }
      case "end" => Future {exit}
      case _ => Future {
        println("Unexpected command")
      }
    }
    Await.result(future,60.seconds)
    run(unit,meh)
  }

  def main(args: Array[String]): Unit = {
    val a = GTDUnit(Nil,Nil,Nil,Nil,Nil,Nil,Nil,Nil)
    val b = new CliReadingMechanism()
    run(a,b)
  }
}