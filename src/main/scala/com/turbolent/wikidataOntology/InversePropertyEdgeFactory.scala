package com.turbolent.wikidataOntology

import com.turbolent.questionCompiler.EdgeContext
import com.turbolent.questionParser.{ListParser, Token}
import Tokens._


object InversePropertyEdgeFactory {

  val factories: Map[String, EdgeFactory] =
    Map("appear" -> P.hasCastMember,
      "marry" -> P.hasSpouse,
      "write" -> P.hasAuthor,
      "direct" -> P.hasDirector,
      "play" -> reverse(P.playsInstrument))

  def stripInitialAuxiliaryVerb(name: Seq[Token]) =
    name match {
      case initial :: rest
        if ListParser.isAuxiliaryVerb(initial) => rest
      case _ => name
    }

}

trait InversePropertyEdgeFactory {

  def makeInversePropertyEdge(name: Seq[Token], node: WikidataNode, context: EdgeContext,
                              env: WikidataEnvironment): WikidataEdge =
  {
    import InversePropertyEdgeFactory._

    val lemmatized = mkLemmaString(stripInitialAuxiliaryVerb(name))
    factories.get(lemmatized) map { _(node, env) } getOrElse {
      throw new RuntimeException(s"No inverse property edge factory for '$lemmatized' " +
                                 s"(${name.mkString(", ")}), context: $context")
    }
  }

}
