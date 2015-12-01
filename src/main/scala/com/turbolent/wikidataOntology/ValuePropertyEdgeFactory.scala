package com.turbolent.wikidataOntology

import com.turbolent.questionCompiler.EdgeContext
import com.turbolent.questionParser.Token
import HaveEdgeFactory.makeHaveEdge
import PrepositionEdgeFactory.{isPrepositionProperty, makePrepositionEdge}
import Tokens._
import scala.collection.mutable


object ValuePropertyEdgeFactory {

  def makeTemporalValueFactory(temporalProperty: Property,
                               alternativeProperty: Property): ContextfulEdgeFactory =
    (node, context, env) => {
      val value = mkWordString(context.value)
      TimeParser.parseTemporal(value) map { _ =>
        out(temporalProperty, node)
      } getOrElse {
        out(alternativeProperty, node)
      }
    }

  val factories: mutable.Map[(String, String), ContextfulEdgeFactory] =
    mutable.Map(
      ("act", "in") -> contextfulReverse(P.hasCastMember),

      ("star", "") -> P.hasCastMember,
      ("star", "in") -> contextfulReverse(P.hasCastMember),

      ("direct", "") -> contextfulReverse(P.hasDirector),
      ("direct", "by") -> P.hasDirector,
      ("be direct", "by") -> P.hasDirector,

      ("write", "") -> contextfulReverse(P.hasAuthor),
      ("write", "by") -> P.hasAuthor,
      ("be write", "by") -> P.hasAuthor,

      ("die", "in") -> makeTemporalValueFactory(P.hasDateOfDeath, P.hasPlaceOfDeath),
      ("die", "on") -> P.hasDateOfDeath,
      ("die", "before") -> P.hasDateOfDeath,
      ("die", "after") -> P.hasDateOfDeath,

      ("bear", "in") -> makeTemporalValueFactory(P.hasDateOfBirth, P.hasPlaceOfBirth),
      ("bear", "on") -> P.hasDateOfBirth,
      ("bear", "before") -> P.hasDateOfBirth,
      ("bear", "after") -> P.hasDateOfBirth,

      ("be bear", "in") -> makeTemporalValueFactory(P.hasDateOfBirth, P.hasPlaceOfBirth),
      ("be bear", "on") -> P.hasDateOfBirth,
      ("be bear", "before") -> P.hasDateOfBirth,
      ("be bear", "after") -> P.hasDateOfBirth,

      ("marry", "") -> P.hasSpouse,

      ("film", "in") -> P.hasFilmingLocation,
      ("be film", "in") -> P.hasFilmingLocation,

      ("speak", "in") -> contextfulReverse(P.hasOfficialLanguage),
      ("be speak", "in") -> contextfulReverse(P.hasOfficialLanguage),
      ("be author", "by") -> P.hasAuthor,
      ("locate", "in") -> { (node, context, env) =>
        out(P.isLocatedIn, node) or
        out(P.country, node) or
        out(P.hasHeadquartersLocation, node)
      },
      ("discover", "") -> contextfulReverse(P.hasDiscovererOrInventor),
      ("discover", "by") -> P.hasDiscovererOrInventor,

      ("invent", "") -> contextfulReverse(P.hasDiscovererOrInventor),
      ("invent", "by") -> P.hasDiscovererOrInventor,

      ("attend", "") -> P.wasEducatedAt,
      ("study", "") -> P.wasEducatedAt,

      ("kill", "") -> contextfulReverse(P.wasKilledBy),

      // NOTE: don't create intermediate node requirement, directly use edge
      ("work", "as") -> { (node, context, env) => node.edge.get },
      ("work", "in") -> P.hasFieldOfWork,

      ("play", "") -> P.playsInstrument
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
