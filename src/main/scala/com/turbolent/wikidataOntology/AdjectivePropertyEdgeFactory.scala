package com.turbolent.wikidataOntology

import com.turbolent.questionCompiler.EdgeContext
import com.turbolent.questionParser.Token
import Tokens._


object AdjectivePropertyEdgeFactory {

  val factories: Map[String, ContextfulEdgeFactory] =
    Map("be high" -> P.hasElevation)

}

trait AdjectivePropertyEdgeFactory {

  def makeAdjectivePropertyEdge(name: Seq[Token], node: WikidataNode, context: EdgeContext,
                                env: WikidataEnvironment): WikidataEdge =
  {
    import AdjectivePropertyEdgeFactory._

    val lemmatized = mkLemmaString(name)
    factories.get(lemmatized) map {
      _(node, context, env)
    } getOrElse {
      throw new RuntimeException(s"No adjective property edge factory for '$lemmatized' " +
                                 s"(${name.mkString(", ")}), context: $context")
    }
  }

}
