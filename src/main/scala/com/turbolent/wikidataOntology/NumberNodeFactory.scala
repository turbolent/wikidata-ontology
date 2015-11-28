package com.turbolent.wikidataOntology

import com.turbolent.numberParser.NumberParser
import com.turbolent.questionCompiler.graph.{LessThanFilter, GreaterThanFilter, Node}
import com.turbolent.questionParser.Token
import Tokens._
import scala.collection.mutable

object NumberNodeFactory {

  type NumberNodeFactory = (Seq[Token], Option[Unit], WikidataEnvironment) => WikidataNode

  val factories: mutable.Map[String, NumberNodeFactory] =
    mutable.Map(
      "in" -> { (name, unit, env) =>
        makeTemporalNode(name, unit)
      },
      "on" -> { (name, unit, env) =>
        makeTemporalNode(name, unit)
      },
      "before" -> {
        (name, unit, env) =>
          val value = makeTemporalNode(name, unit)
          env.newNode().filter(LessThanFilter(value))
      },
      "after" -> {
        (name, unit, env) =>
          val value = makeTemporalNode(name, unit)
          env.newNode().filter(GreaterThanFilter(value))
      },
      "less than" -> {
        (name, unit, env) =>
          val value = makeNumberUnitNode(name, unit)
          env.newNode().filter(LessThanFilter(value))
      },
      "more than" -> {
        (name, unit, env) =>
          val value = makeNumberUnitNode(name, unit)
          env.newNode().filter(GreaterThanFilter(value))
      })

  val units: mutable.Map[String, Unit] =
    mutable.Map(
      "meter" -> U.meter,
      "second" -> U.second)

  def makeNumberUnitNode(name: Seq[Token], unit: Option[Unit]): WikidataNode = {
    val words = mkWordString(name)
    val number = NumberParser.parse(words)
    Node(unit map {
      NumberWithUnitLabel(number, _)
    } getOrElse {
      NumberLabel(number)
    })
  }

  def makeTemporalNode(name: Seq[Token], optionalUnit: Option[Unit]): WikidataNode = {
    val words = mkWordString(name)
    TimeParser.parseTemporal(words) map { temporal =>
      temporalAsNode(temporal)
    } getOrElse {
      makeNumberUnitNode(name, optionalUnit)
    }
  }

}


trait NumberNodeFactory {

  def makeNumberNode(name: Seq[Token], unitName: Seq[Token], filter: Seq[Token],
                     env: WikidataEnvironment): WikidataNode =
  {
    import NumberNodeFactory._

    val unit =
      if (unitName.isEmpty) None
      else units.get(mkLemmaString(unitName))
    if (filter.isEmpty)
      makeNumberUnitNode(name, unit)
    else {
      val filterWords = mkWordString(filter)
      factories.get(filterWords) map {
        _(name, unit, env)
      } getOrElse {
        makeNumberUnitNode(name, unit)
      }
    }
  }

}
