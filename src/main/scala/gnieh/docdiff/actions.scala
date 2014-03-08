package gnieh.docdiff

sealed trait Action

final case class Insert(node: Node, father: Node, position: Int) extends Action

final case class Delete(node: Node) extends Action

final case class Update(node: Node, element: Node) extends Action

final case class Move(node: Node, father: Node, position: Int) extends Action

