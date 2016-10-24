/**
  * Created by fatihdonmez on 24/10/16.
  */
trait Email {

  def body: String

}

case class EmailImpl(body: String) extends Email


abstract class EmailDecorator extends Email {

  protected val email: Email
}

case class ExternalEmailDecorator(protected override val email: Email) extends EmailDecorator {

  override def body: String = email.body + disclaimer

  private val disclaimer: String =
    """
      |************************************************************************
      |The information contained in this message or any of its attachments may be confidential and is intended for the exclusive use of the addressee(s).
      |Any disclosure, reproduction, distribution or other dissemination or use of this communication is strictly prohibited without the express permission of the sender.
      |The views expressed in this email are those of the individual and not necessarily those of BigBadAssCorp or BigBadAssCorp affiliated companies.
      |BigBadAssCorp email is for business use only.
      |This email and any response may be monitored by BigBadAssCorp to be in compliance with BigBadAssCorp's global policies and standards
    """.stripMargin

}


object EmailSender {

  def send(to: String, email: Email): Unit = {

    if(!to.endsWith("@bigbadass.com")) {

      val decorator = ExternalEmailDecorator(email)
      send(to, decorator.body)
    } else {
      send(to, email.body)
    }
  }

  def send(to: String, body: String): Unit = {
    //Send to email client
    println(s"$to -> $body")
  }

  def disclaimer =
    """
      |************************************************************************
      |The information contained in this message or any of its attachments may be confidential and is intended for the exclusive use of the addressee(s).
      |Any disclosure, reproduction, distribution or other dissemination or use of this communication is strictly prohibited without the express permission of the sender.
      |The views expressed in this email are those of the individual and not necessarily those of BigBadAssCorp or BigBadAssCorp affiliated companies.
      |BigBadAssCorp email is for business use only.
      |This email and any response may be monitored by BigBadAssCorp to be in compliance with BigBadAssCorp's global policies and standards
    """.stripMargin

  def emailMaker(func: => String) = {
    (email: Email) => {
      val strToAppend = func
      EmailImpl(email.body + strToAppend)
    }
  }

  val disclaimerDecorator = emailMaker(disclaimer)

  def sendFunctional(to: String, email: Email) = {

    if(!to.endsWith("@bigbadass.com")) {

      val newEmail = disclaimerDecorator(email)
      send(to, newEmail.body)
    } else {
      send(to, email.body)
    }

  }

  def main(args: Array[String]) = {

    //Decorator
    send("test@bigbadass.com", EmailImpl("Selam naber"))

    //Functional
    sendFunctional("test@gmail.com", EmailImpl("Selam naber"))
  }
}
