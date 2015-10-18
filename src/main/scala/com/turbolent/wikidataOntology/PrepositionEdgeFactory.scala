package com.turbolent.wikidataOntology

import com.turbolent.questionCompiler.{PersonSubject, NamedSubject, ThingSubject, EdgeContext}
import com.turbolent.questionParser.Token
import Tokens._


object PrepositionEdgeFactory {

  val namedFactories: Map[(String, String), ContextfulEdgeFactory] =
    Map(("city", "in") -> P.isLocatedIn,
      ("book", "by") -> P.hasAuthor)

  val personFactories: Map[String, ContextfulEdgeFactory] = Map()

  val thingFactories: Map[String, ContextfulEdgeFactory] = Map()

  def isPrepositionProperty(name: Seq[Token], filter: Seq[Token]) =
    (name, filter) match {
      case (Nil, Seq(Token(_, "IN"))) => true
      case _ => false
    }

  def makePrepositionEdge(node: WikidataNode, context: EdgeContext,
                          env: WikidataEnvironment): WikidataEdge =
  {
    val lemmatizedFilter = mkLemmaString(context.filter)
    val factory = context.subject match {
      case PersonSubject =>
        personFactories.get(lemmatizedFilter)
      case ThingSubject =>
        thingFactories.get(lemmatizedFilter)
      case NamedSubject(subjectName) =>
        // TODO: adjectives
        val (_, subjectNameRest) = splitName(subjectName)
        val key = (mkLemmaString(subjectNameRest),
            lemmatizedFilter)
        namedFactories.get(key)
    }

    factory map {
      _(node, context, env)
    } getOrElse {
      val message = s"No preposition property edge factory for context: $context"
      throw new RuntimeException(message)
    }
  }

}
