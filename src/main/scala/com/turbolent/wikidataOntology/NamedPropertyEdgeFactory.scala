package com.turbolent.wikidataOntology

import com.turbolent.questionCompiler.Subject
import com.turbolent.questionParser.Token
import Tokens._


object NamedPropertyEdgeFactory {

  val factories: Map[String, EdgeFactory] =
    Map("direct" -> reverse(P.hasDirector),
      "write" -> reverse(P.hasAuthor),
      "marry" -> P.hasSpouse,
      "die" -> { (node, env) =>
        out(P.hasDateOfDeath, node) or
        out(P.hasPlaceOfDeath, node)
      })

}


trait NamedPropertyEdgeFactory {

  def makeNamedPropertyEdge(name: Seq[Token], node: WikidataNode, subject: Subject,
                            env: WikidataEnvironment): WikidataEdge =
  {
    import NamedPropertyEdgeFactory._

    val lemmatized = mkLemmaString(name)
    factories.get(lemmatized) map {
      _(node, env)
    } getOrElse {
      throw new RuntimeException(s"No plain property edge factory for '$lemmatized' " +
                                 s"(${name.mkString(", ")})")
    }
  }

}
