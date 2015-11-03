package com.turbolent.wikidataOntology

import com.turbolent.questionCompiler.graph.{LessThanFilter, GreaterThanFilter}
import com.turbolent.questionCompiler.{PersonSubject, NamedSubject, ThingSubject, EdgeContext}
import com.turbolent.questionParser.Token
import HaveEdgeFactory.makeHaveEdge
import Tokens._


object ComparativePropertyEdgeFactory {

  type FilterFactory = (WikidataNode) => WikidataFilter

  def makeComparisonFactory(property: Property, filterFactory: FilterFactory): ContextfulEdgeFactory =
    (node, context, env) => {
      val otherArea = env.newNode()
          .in(node, property)
      val filter = filterFactory(otherArea)
      val area = env.newNode()
          .filter(filter)
      out(property, area)
    }

  val namedFactories: Map[(String, String, String), ContextfulEdgeFactory] =
    Map(("city", "be", "bigger than") ->
        makeComparisonFactory(P.hasArea, GreaterThanFilter(_)),
      ("city", "live", "more than") -> P.hasPopulation)

  val thingFactories: Map[(String, String), ContextfulEdgeFactory] = Map()

  val personFactories: Map[(String, String), ContextfulEdgeFactory] = Map(
    // TODO: improve: take into account death date
    ("be", "older than") -> makeComparisonFactory(P.hasDateOfBirth, LessThanFilter(_)))

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
