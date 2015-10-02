package com.turbolent.wikidataOntology

import java.time.temporal.Temporal


trait NodeLabel

case class VarLabel(id: Int) extends NodeLabel {
  override def toString = "_" + id
}

case class ItemLabel(item: Item) extends NodeLabel {
  override def toString = item.toString
}

case class ValueLabel(value: String) extends NodeLabel

case class NumberLabel(value: Double) extends NodeLabel

case class NumberWithUnitLabel(value: Double, unit: Unit) extends NodeLabel

case class TemporalLabel(temporal: Temporal) extends NodeLabel
