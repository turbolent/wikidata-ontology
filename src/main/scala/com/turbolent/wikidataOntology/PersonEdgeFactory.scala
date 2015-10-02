package com.turbolent.wikidataOntology


trait PersonEdgeFactory {

  def makePersonEdge(env: WikidataEnvironment) =
    out(P.isA, Q.human)

}
