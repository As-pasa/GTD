package domain

import java.util.Date

sealed trait CardType

case object GtdUndefined extends CardType

case object GtdNote extends CardType

case object GtdTrash extends CardType

case object GtdDelegated extends CardType

case object GtdCalendar extends CardType

case object GtdLater extends CardType

case object GtdNow extends CardType

case object GtdProject extends CardType

case class GenericGtdCard(name: String,
                          description: String,
                          cardType: CardType,
                          dateOfStart: Option[Date],
                          deadline: Option[Date],
                          delegatedTo: Option[String],
                          plan: Option[List[String]],
                          criteria: Option[String],
                          firstStep: Option[String]
                         )

sealed trait GtdCard {
  def name: String

  def description: String
}

case class GtdThought(name: String) extends GtdCard {
  override def description: String = ""
}

case class Note(name: String, description: String) extends GtdCard

case class DelegatedTask(name: String, descr: String, delegated: String, until: Date) extends GtdCard {
  override def description: String = descr + s"\nDELEGATE:[$delegated] UNTIL [${until.toString}]"
}

case class TimePinnedTask(name: String, descr: String, startTime: Date) extends GtdCard {
  override def description: String = descr + s"\nPLANNED ON: [${startTime.toString}]"
}

case class LaterTask(name: String, description: String) extends GtdCard

case class NowTask(name: String, description: String) extends GtdCard

case class ProjectTask(name: String, descr: String, plan: List[String], criteria: String, firstStep: String) extends GtdCard {
  override def description: String = descr + s"\nCRITERIA: [${criteria}] \nPLAN: [${plan.mkString("\n- ")}]\nFIRST STEP: ${firstStep}"
}


