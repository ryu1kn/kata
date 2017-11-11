package example

object PasswordInspector {

  def decompose(attempt: String, passwords: List[String]): Option[List[String]] = {
    val exactMatch = passwords.find(_ == attempt)
    exactMatch match {
      case Some(m) => Some(List(m))
      case _ =>
        if (attempt == passwords.mkString) Some(passwords)
        else if (attempt == passwords.reverse.mkString) Some(passwords.reverse)
        else findFullGrownBranch(attempt, gradualDecompose(attempt, passwords))
    }
  }

  private case class Decomp(prefix: String, rest: List[Decomp])

  private def gradualDecompose(attempt: String, passwords: List[String]): Decomp = {
    def gradualDecompose_(attempt: String): List[Decomp] =
      passwords.filter(attempt.startsWith)
        .map(password => Decomp(password, gradualDecompose_(attempt.stripPrefix(password))))
    Decomp("", gradualDecompose_(attempt))
  }

  private def findFullGrownBranch(attempt: String, node: Decomp): Option[List[String]] = {
    def reconstructAttempts(decomp: Decomp): List[List[String]] = decomp match {
      case Decomp(password, List()) => List(List(password))
      case Decomp(password, decomps) => decomps.flatMap(reconstructAttempts).map(password :: _)
    }
    reconstructAttempts(node).find(_.mkString == attempt) match {
      case Some("" :: rest) => Some(rest)
      case _ => None
    }
  }

}
