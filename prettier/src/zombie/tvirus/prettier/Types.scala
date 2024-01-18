package zombie.tvirus.prettier

enum Description:
  case Nil()
  case Line()
  case Text(text: String)
  case Concat(left: Description, right: Description)
  case Union(left: Description, right: Description)
  case Nest(indent: Int, description: Description)

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