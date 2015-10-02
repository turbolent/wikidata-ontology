package com.turbolent.wikidataOntology

import com.turbolent.questionCompiler.graph.{Max, Count}
import com.turbolent.questionParser.Token
import AdjectiveEdgeFactory.makeAdjectiveEdge
import Tokens._


object ValueNodeFactory {

  val factories: Map[String, NodeFactory] =
    Map("movie" -> Q.movie,
      "actor" -> Q.actor,
      "mountain" -> Q.mountain,
      "president" -> Q.president,
      "author" -> { (node, env) =>
        node.in(env.newNode(), P.hasAuthor)
      },
      "musician" -> Q.musician,
      "book" -> Q.book,
      "language" -> Q.language,
      "instrument" -> Q.musicalInstrument,
      "city" -> Q.city,
      "child" -> { (node, env) =>
        node.in(env.newNode(), P.hasChild)
      },
      "planet" -> Q.planet)

  val adjectiveFactories: Map[String, NodeFactory] =
    Map("most" -> {
      (node, env) =>
        node.aggregate(Count).aggregate(Max)
    })

  def wrapAdjective(adjectives: Seq[Token], node: WikidataNode,
                    env: WikidataEnvironment): WikidataNode =
    if (adjectives.isEmpty) node
    else {
      val adjectiveWords = mkWordString(adjectives)
      adjectiveFactories.get(adjectiveWords)
          .map(_(node, env))
          .getOrElse(node)
    }

}


trait ValueNodeFactory {


  def makeValueNode(name: Seq[Token], filter: Seq[Token],
                    env: WikidataEnvironment): WikidataNode =
  {
    import ValueNodeFactory._

    val (adjectives, nameRest) = splitName(name)
    val lemmatized = mkLemmaString(nameRest)

    val node = env.newNode()
    factories.get(lemmatized) map { factory =>
      val result = factory(node, env)
      makeAdjectiveEdge(adjectives, lemmatized, env) map {
        result.and
      } getOrElse {
        wrapAdjective(adjectives, result, env)
      }
    } getOrElse {
      val words = mkWordString(name)
      node.out(NameLabel, words)
    }
  }

}
