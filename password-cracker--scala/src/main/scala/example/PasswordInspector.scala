package example

object PasswordInspector {

  private lazy val TREE_ROOT_WORD = ""

  def decompose(attempt: String, passwords: List[String]): Option[List[String]] =
    findFullGrownBranch(attempt, buildWordTree(attempt, passwords))

  private case class WordTree(prefix: String, rest: List[WordTree])

  private def buildWordTree(attempt: String, passwords: List[String]): WordTree = {
    def buildWordTree_(attempt: String): List[WordTree] =
      passwords
        .filter(attempt.startsWith)
        .map(password => WordTree(password, buildWordTree_(attempt.stripPrefix(password))))

    WordTree(TREE_ROOT_WORD, buildWordTree_(attempt))
  }

  private def findFullGrownBranch(attempt: String, tree: WordTree): Option[List[String]] = {
    def flattenWordTree(tree: WordTree): List[List[String]] = tree match {
      case WordTree(password, List()) => List(List(password))
      case WordTree(password, subTree) => subTree.flatMap(flattenWordTree).map(password :: _)
    }

    flattenWordTree(tree)
      .map { case TREE_ROOT_WORD :: tail => tail }
      .find(_.mkString == attempt)
  }

}
