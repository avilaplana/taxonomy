package exercise

case class Translation(iso: String, value: String) {

  private[exercise] def toCSV = s"$iso,$value"
}

object Translation {

  def apply(translations: String): Seq[Translation] =
    translations.split(",").sliding(2, 2).map(t => Translation(t.head, t.last)).toSeq
}

case class Tag(value: String, translation: Seq[Translation]) {

  private[exercise] def toCSV = s"$value,${translation.map(_.toCSV).mkString(",")}"
}

object Tag {

  def apply(value: String, translations: String): Tag = Tag(value, Translation(translations))
}
