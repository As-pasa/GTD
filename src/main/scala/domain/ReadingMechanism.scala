import domain.{GenericGtdCard, GtdCalendar, GtdDelegated, GtdLater, GtdNote, GtdNow, GtdProject, GtdThought, GtdTrash}

import scala.concurrent.ExecutionContext.Implicits.global
import java.util.Date
import scala.concurrent.Future

trait ReadingMechanism {
  def askFreeQuestion(q: String): Future[String]

  def askTimeQuestion(q: String): Future[Date]

  def askSelectiveQuestion(q: String, l: List[String]): Future[String]

  def shout(message: String): Unit
}

object CardConstructors {
  def proceedStaticTasks(name: String, descr: String, meh: ReadingMechanism): Future[GenericGtdCard] = for {
    is_it_trash <- meh.askSelectiveQuestion("Is it trash?", "Yes" :: "No" :: Nil)

  } yield if (is_it_trash == "Yes") GenericGtdCard(name, descr, GtdTrash, None, None, None, None, None, None)
  else GenericGtdCard(name, descr, GtdNote, None, None, None, None, None, None)

  def constructDelegate(name: String, descr: String, meh: ReadingMechanism): Future[GenericGtdCard] = for {
    slave <- meh.askFreeQuestion("to whom we will delegate task?")
    time <- meh.askTimeQuestion("when will we ask for it?")
  } yield GenericGtdCard(name, descr, GtdDelegated, None, Some(time), Some(slave), None, None, None)

  def constructCalendar(name: String, descr: String, meh: ReadingMechanism): Future[GenericGtdCard] = for {
    time <- meh.askTimeQuestion("when will this task become actual?")
  } yield GenericGtdCard(name, descr, GtdCalendar, Some(time), None, None, None, None, None)

  def wrap(genericGtdCard: GenericGtdCard): Future[GenericGtdCard] = Future.successful(genericGtdCard)

  def proceedLaterTasks(name: String, descr: String, meh: ReadingMechanism): Future[GenericGtdCard] = for {
    is_it_sceduled <- meh.askSelectiveQuestion("Is it pinned to date?", "Yes" :: "No" :: Nil)
    card <- if (is_it_sceduled == "Yes") constructCalendar(name, descr, meh) else wrap(GenericGtdCard(name, descr, GtdLater, None, None, None, None, None, None))
  } yield card

  def constructProject(name: String, descr: String, meh: ReadingMechanism): Future[GenericGtdCard] = for {
    criteria <- meh.askFreeQuestion("describe criteria of success in this project")
    plan <- meh.askFreeQuestion("describe approximate plan for this project. Split steps with .")

  } yield {
    val steps = plan.split(".").toList
    GenericGtdCard(name, descr, GtdProject, None, None, None, Some(steps), Some(criteria), steps.headOption)

  }

  def processEasyTasks(name: String, descr: String, meh: ReadingMechanism): Future[GenericGtdCard] = for {
    is_it_2_min_task <- meh.askSelectiveQuestion("can it be done in 2 minutes?", "Yes" :: "No" :: Nil)
    card <- if (is_it_2_min_task == "Yes") {
      meh.shout(s"URGENT TASK: [$name]\n DESCRIPTION:\n ${descr}\n DO IT OR LOSE IT");
      wrap(GenericGtdCard(name, descr, GtdTrash, None, None, None, None, None, None))
    }
    else wrap(GenericGtdCard(name, descr, GtdNow, None, None, None, None, None, None))
  } yield card

  def proceedUrgentTasks(name: String, descr: String, meh: ReadingMechanism): Future[GenericGtdCard] = for {
    is_it_complex_one <- meh.askSelectiveQuestion("is it complex task?", "Yes" :: "No" :: Nil)
    card <- if (is_it_complex_one == "Yes") constructProject(name, descr, meh) else processEasyTasks(name, descr, meh)
  } yield card

  def proceedOurTasks(name: String, descr: String, meh: ReadingMechanism): Future[GenericGtdCard] = for {
    is_it_urgent <- meh.askSelectiveQuestion("Should we do it now?", "Yes" :: "No" :: Nil)
    card <- if (is_it_urgent == "Yes") proceedUrgentTasks(name, descr, meh) else proceedLaterTasks(name, descr, meh)
  } yield card

  def proceedDynamicTasks(name: String, descr: String, meh: ReadingMechanism): Future[GenericGtdCard] = for {
    can_be_delegated <- meh.askSelectiveQuestion("Can it be delegated?", "Yes" :: "No" :: Nil)
    card <- if (can_be_delegated == "Yes") constructDelegate(name, descr, meh) else proceedOurTasks(name, descr, meh)
  } yield card

  def constructGenericCard(thought: GtdThought, meh: ReadingMechanism): Future[GenericGtdCard] = for {
    descr <- meh.askFreeQuestion(s"What is it? [${thought.name}]")
    shouldIDoAnything <- meh.askSelectiveQuestion("Should we do anything with this?", "Yes" :: "No" :: Nil)
    card <- if (shouldIDoAnything == "Yes") proceedDynamicTasks(thought.name, descr, meh) else proceedStaticTasks(thought.name, descr, meh)
  } yield card

}