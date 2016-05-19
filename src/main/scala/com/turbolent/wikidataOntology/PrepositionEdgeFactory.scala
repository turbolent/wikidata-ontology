package com.turbolent.wikidataOntology

import com.turbolent.questionCompiler.{PersonSubject, NamedSubject, ThingSubject, EdgeContext}
import com.turbolent.questionParser.Token
import Tokens._
import scala.collection.mutable

object PrepositionEdgeFactory {

  val namedFactories: mutable.Map[(String, String), ContextfulEdgeFactory] =
    mutable.Map(
      ("city", "in") -> P.isLocatedIn,
      ("book", "by") -> P.hasAuthor,
      ("movie", "by") -> P.hasDirector
    )

  val personFactories: mutable.Map[String, ContextfulEdgeFactory] =
    mutable.Map()

  val thingFactories: mutable.Map[String, ContextfulEdgeFactory] =
    mutable.Map()

  def isPrepositionProperty(name: Seq[Token], filter: Seq[Token]) =
    (name, filter) match {
      case (Nil, Seq(Token(_, "IN", _))) => true
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
