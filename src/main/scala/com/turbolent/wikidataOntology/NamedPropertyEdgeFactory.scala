package com.turbolent.wikidataOntology

import com.turbolent.questionCompiler.Subject
import com.turbolent.questionParser.Token
import Tokens._
import scala.collection.mutable

object NamedPropertyEdgeFactory {

  val factories: mutable.Map[String, EdgeFactory] =
    mutable.Map(
      "direct" -> reverse(P.hasDirector),
      "write" -> reverse(P.hasAuthor),
      "marry" -> P.hasSpouse,
      "be marry" -> P.hasSpouse,
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
      throw new RuntimeException(s"No named property edge factory for '$lemmatized' " +
                                 s"(${name.mkString(", ")})")
    }
  }

}
