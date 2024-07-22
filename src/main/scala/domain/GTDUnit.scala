package domain

trait UnitError {
  def message: String
}

case class GenericError(message: String) extends UnitError

case class GTDUnit(var basket: List[GtdThought],
                   var notes: List[Note],
                   var trash: List[GtdCard],
                   var delegatedTasks: List[DelegatedTask],
                   var calendar: List[TimePinnedTask],
                   var later: List[LaterTask],
                   var now: List[NowTask],
                   var projects: List[ProjectTask]) {
  def put(card: GenericGtdCard): Option[UnitError] = {
    card.cardType match {
      case GtdTrash => {
        trash = GtdThought(card.name) :: trash;
        None
      }
      case GtdLater => {
        later = LaterTask(card.name, card.description) :: later;
        None
      }
      case GtdNow => {
        now = NowTask(card.name, card.description) :: now;
        None
      }
      case GtdCalendar => {
        val data = for {
          time <- card.dateOfStart
        } yield TimePinnedTask(card.name, card.description, time)
        data match {
          case Some(value) => {
            calendar = calendar.appended(value);
            None
          }
          case None => Some(GenericError("No date of start found on time pinned card"))
        }
      }
      case GtdUndefined => {
        basket = basket.appended(GtdThought(card.name)); None
      }
      case GtdNote => {
        notes = notes.appended(Note(card.name, card.description)); None
      }
      case GtdDelegated => {
        val data = for {
          time <- card.deadline
          delegatedTo <- card.delegatedTo
        } yield DelegatedTask(card.name, card.description, delegatedTo, time)
        data match {
          case Some(value) => {
            delegatedTasks = delegatedTasks.appended(value); None
          }
          case None => Some(GenericError("No Delegate or Deadline on delegatedTask"))
        }
      }
      case GtdProject => {
        (for {
          criteria <- card.criteria
          plan <- card.plan
          first_step <- card.firstStep
        } yield ProjectTask(card.name, card.description, plan, criteria, first_step)) match {
          case Some(value) => {
            projects = projects.appended(value); None
          }
          case None => Some(GenericError("criteria or plan or first step not specified on project task"))
        }

      }
    }
  }
}

