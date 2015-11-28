package com.turbolent.wikidataOntology

import com.turbolent.questionCompiler.graph.Max
import com.turbolent.questionParser.Token
import Tokens._
import scala.collection.mutable


object AdjectiveEdgeFactory {

  type PlainEdgeFactory = (WikidataEnvironment) => WikidataEdge

  def maxPopulation(env: WikidataEnvironment) =
    out(P.hasPopulation,
      env.newNode().aggregate(Max))

  val factories: mutable.Map[(String, String), PlainEdgeFactory] =
    mutable.Map(
      ("largest", "city") -> maxPopulation,
      ("biggest", "city") -> maxPopulation)

  def makeAdjectiveEdge(adjectives: Seq[Token], lemmatized: String,
                        env: WikidataEnvironment): Option[WikidataEdge] =
    if (adjectives.isEmpty) None
    else {
      val adjectiveWords = mkWordString(adjectives)
      val key = (adjectiveWords, lemmatized)
      factories.get(key).map(_(env))
    }

}
