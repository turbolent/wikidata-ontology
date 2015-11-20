package com.turbolent.wikidataOntology

import com.turbolent.questionCompiler.EdgeContext
import com.turbolent.questionParser.Token
import HaveEdgeFactory.makeHaveEdge
import PrepositionEdgeFactory.{isPrepositionProperty, makePrepositionEdge}
import Tokens._


object ValuePropertyEdgeFactory {

  val factories: Map[(String, String), ContextfulEdgeFactory] =
    Map(
      ("act", "in") -> contextfulReverse(P.hasCastMember),

      ("star", "") -> P.hasCastMember,
      ("star", "in") -> contextfulReverse(P.hasCastMember),

      ("direct", "") -> contextfulReverse(P.hasDirector),
      ("direct", "by") -> P.hasDirector,

      ("write", "") -> contextfulReverse(P.hasAuthor),
      ("be write", "by") -> P.hasAuthor,

      // TODO: or P.hasDateOfDeath
      ("die", "in") -> P.hasPlaceOfDeath,
      ("die", "on") -> P.hasDateOfDeath,
      ("die", "before") -> P.hasDateOfDeath,
      ("die", "after") -> P.hasDateOfDeath,

      // TODO: or P.hasDateOfBirth
      ("bear", "in") -> P.hasPlaceOfBirth,
      ("bear", "on") -> P.hasDateOfBirth,
      ("bear", "before") -> P.hasDateOfBirth,
      ("bear", "after") -> P.hasDateOfBirth,

      // TODO: or P.hasDateOfBirth
      ("be bear", "in") -> P.hasPlaceOfBirth,
      ("be bear", "on") -> P.hasDateOfBirth,
      ("be bear", "before") -> P.hasDateOfBirth,
      ("be bear", "after") -> P.hasDateOfBirth,

      ("marry", "") -> P.hasSpouse,
      ("be film", "in") -> P.hasFilmingLocation,
      ("be speak", "in") -> contextfulReverse(P.hasOfficialLanguage),
      ("be author", "by") -> P.hasAuthor,
      ("locate", "in") -> P.isLocatedIn,
      ("discover", "") -> contextfulReverse(P.hasDiscovererOrInventor),
      ("invent", "") -> contextfulReverse(P.hasDiscovererOrInventor)
    )

}


trait ValuePropertyEdgeFactory {

  def makeValuePropertyEdge(propertyName: Seq[Token], node: WikidataNode,
                            context: EdgeContext, env: WikidataEnvironment): WikidataEdge =
  {
    import ValuePropertyEdgeFactory._

    val lemmatizedProperty = mkLemmaString(propertyName)

    if (lemmatizedProperty == "have")
      return makeHaveEdge(node, context, env)

    if (isPrepositionProperty(propertyName, context.filter))
      return makePrepositionEdge(node, context, env)

    val lemmatizedFilter = mkLemmaString(context.filter)
    val key = (lemmatizedProperty, lemmatizedFilter)
    factories.get(key) map {
      _(node, context, env)
    } getOrElse {
      val message = s"No value property edge factory for '$lemmatizedProperty' " +
                    s"(${propertyName.mkString(", ")}), context: $context"
      throw new RuntimeException(message)
    }
  }

}
