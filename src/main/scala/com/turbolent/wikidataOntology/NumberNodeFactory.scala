package com.turbolent.wikidataOntology

import java.time.Year
import java.time.temporal.Temporal

import com.turbolent.numberParser.NumberParser
import com.turbolent.questionCompiler.graph.{LessThanFilter, GreaterThanFilter, Node}
import com.turbolent.questionParser.Token
import Tokens._


object NumberNodeFactory {

  type NumberNodeFactory = (Seq[Token], Option[Unit], WikidataEnvironment) => WikidataNode

  val factories: Map[String, NumberNodeFactory] =
    Map("before" -> {
      (name, unit, env) => {
        val value = makeTemporalNode(name, unit)
        env.newNode().filter(LessThanFilter(value))
      }
    },
      "after" -> {
        (name, unit, env) => {
          val value = makeTemporalNode(name, unit)
          env.newNode().filter(GreaterThanFilter(value))
        }
      },
      "less than" -> {
        (name, unit, env) => {
          val value = makeNumberUnitNode(name, unit)
          env.newNode().filter(LessThanFilter(value))
        }
      },
      "more than" -> {
        (name, unit, env) => {
          val value = makeNumberUnitNode(name, unit)
          env.newNode().filter(GreaterThanFilter(value))
        }
      })

  val units: Map[String, Unit] =
    Map("meter" -> U.meter,
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

  val yearPattern = """[12]\d{3}""".r

  // TODO: extend
  def parseTemporal(name: String): Option[Temporal] =
    name match {
      case yearPattern(_*) => Some(Year.parse(name))
      case _ => None
    }

  def makeTemporalNode(name: Seq[Token], optionalUnit: Option[Unit]): WikidataNode = {
    val words = mkWordString(name)
    parseTemporal(words) map { temporal =>
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
