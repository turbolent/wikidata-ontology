package com.turbolent.wikidataOntology

import com.turbolent.questionCompiler.graph.{LessThanFilter, GreaterThanFilter}
import com.turbolent.questionCompiler.{PersonSubject, NamedSubject, ThingSubject, EdgeContext}
import com.turbolent.questionParser.Token
import HaveEdgeFactory.makeHaveEdge
import Tokens._
import scala.collection.mutable

object ComparativePropertyEdgeFactory {

  type FilterFactory = (WikidataNode) => WikidataFilter

  def makeComparisonFactory(property: Property, filterFactory: FilterFactory): ContextfulEdgeFactory =
    (node, context, env) => {
      val otherValue = env.newNode()
          .in(node, property)
      val filter = filterFactory(otherValue)
      val value = env.newNode()
          .filter(filter)
      out(property, value)
    }

  val namedFactories: mutable.Map[(String, String, String), ContextfulEdgeFactory] =
    mutable.Map(
      ("city", "be", "big than") -> makeComparisonFactory(P.hasArea, GreaterThanFilter(_)),
      ("city", "be", "small than") -> makeComparisonFactory(P.hasArea, LessThanFilter(_)),
      // NOTE: comparison handled in NumberNodeFactory
      ("city", "live", "more than") -> P.hasPopulation,
      ("city", "live", "less than") -> P.hasPopulation)


  val thingFactories: mutable.Map[(String, String), ContextfulEdgeFactory] =
    mutable.Map()

  val personFactories: mutable.Map[(String, String), ContextfulEdgeFactory] =
    mutable.Map(
      // TODO: improve: take into account death date
      ("be", "old than") -> makeComparisonFactory(P.hasDateOfBirth, LessThanFilter(_)))

}

trait ComparativePropertyEdgeFactory {

  def makeComparativePropertyEdge(name: Seq[Token], node: WikidataNode, context: EdgeContext,
                                  env: WikidataEnvironment): WikidataEdge =
  {
    import ComparativePropertyEdgeFactory._

    val lemmatizedProperty = mkLemmaString(name)

    if (lemmatizedProperty == "have")
      return makeHaveEdge(node, context, env)

    val lemmatizedFilter = mkLemmaString(context.filter)

    val factory = context.subject match {
      case PersonSubject =>
        val key = (lemmatizedProperty, lemmatizedFilter)
        personFactories.get(key)

      case ThingSubject =>
        val key = (lemmatizedProperty, lemmatizedFilter)
        thingFactories.get(key)

      case NamedSubject(subjectName) =>
        // TODO: adjectives
        val (_, subjectNameRest) = splitName(subjectName)
        val lemmatizedSubject = mkLemmaString(subjectNameRest)
        val key = (lemmatizedSubject,
            lemmatizedProperty,
            lemmatizedFilter)
        namedFactories.get(key)
    }

    factory map {
      _(node, context, env)
    } getOrElse {
      val message = s"No comparative property edge factory for '$lemmatizedProperty' " +
                    s"(${name.mkString(", ")}), context: $context"
      throw new RuntimeException(message)
    }
  }

}
