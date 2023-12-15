package zombie.tvirus.prettier

extension (left: Description)
  def <> (right: Description) = Concat(left, right)

  def <+> (right: Description) = left <> Text(" ") <> right

  def </> (right: Description) = left <> Line() <> right

  def <+/> (right: Description) = left <> Union(Text(" "), Line()) <> right

def bracket(left: Description, body: Description, right: Description) =
  Union(left <> body <> right, left <> Nest(2, Line() <> body) </> right)

def bracketStr(left: String, body: Description, right: String) =
  bracket(Text(left), body, Text(right))
