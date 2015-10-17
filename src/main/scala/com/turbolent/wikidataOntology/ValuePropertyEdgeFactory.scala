package com.turbolent.wikidataOntology

import com.turbolent.questionCompiler.EdgeContext
import com.turbolent.questionParser.Token
import HaveEdgeFactory.makeHaveEdge
import PrepositionEdgeFactory.{isPrepositionProperty, makePrepositionEdge}
import Tokens._


object ValuePropertyEdgeFactory {

  val factories: Map[(String, String), EdgeFactory] =
    Map(("act", "in") -> reverse(P.hasCastMember),
      ("direct", "by") -> P.hasDirector,
      ("star", "in") -> reverse(P.hasCastMember),
      ("star", "") -> P.hasCastMember,
      ("direct", "") -> reverse(P.hasDirector),
      ("write", "") -> reverse(P.hasAuthor),
      ("die", "in") -> P.hasPlaceOfDeath,
      ("be bear", "in") -> P.hasPlaceOfBirth,
      ("bear", "in") -> P.hasPlaceOfBirth,
      ("be bear", "before") -> P.hasDateOfBirth,
      ("marry", "") -> P.hasSpouse,
      // TODO: or date
      ("be film", "in") -> P.hasFilmingLocation,
      ("be speak", "in") -> reverse(P.hasOfficialLanguage),
      ("be author", "by") -> P.hasAuthor,
      ("be write", "by") -> P.hasAuthor,
      ("locate", "in") -> P.isLocatedIn,
      ("discover", "") -> reverse(P.hasDiscovererOrInventor),
      ("invent", "") -> reverse(P.hasDiscovererOrInventor)
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
      _(node, env)
    } getOrElse {
      val message = s"No value property edge factory for '$lemmatizedProperty' " +
                    s"(${propertyName.mkString(", ")}), context: $context"
      throw new RuntimeException(message)
    }
  }

}
