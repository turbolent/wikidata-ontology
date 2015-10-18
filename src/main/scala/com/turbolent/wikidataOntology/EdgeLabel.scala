package com.turbolent.wikidataOntology


trait EdgeLabel

case class PropertyLabel(property: Property) extends EdgeLabel {
  override def toString = property.toString
}

case object NameLabel extends EdgeLabel {
  override def toString = "name"
}

case object YearLabel extends EdgeLabel {
  override def toString = "year"
}
