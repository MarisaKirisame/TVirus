package zombie.tvirus.prettier

sealed abstract class Description

case class Nil() extends Description
case class Gap() extends Description
case class Text(text: String) extends Description
case class Concat(left: Description, right: Description) extends Description
case class Union(left: Description, right: Description) extends Description
case class Nest(indent: Int, description: Description) extends Description

// Document is made for laziness
sealed abstract class Document {
  lazy val cont: Document
}

case class DNil() extends Document {
  lazy val cont = DNil()
}
case class DText(text: String, _cont: ()=>Document) extends Document {
  lazy val cont = _cont()
}
case class DLine(indent: Int, _cont: ()=>Document) extends Document {
  lazy val cont = _cont()
}