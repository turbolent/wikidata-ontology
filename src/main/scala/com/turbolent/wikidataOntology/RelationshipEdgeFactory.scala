package com.turbolent.wikidataOntology

import com.turbolent.questionParser.Token
import AdjectiveEdgeFactory.makeAdjectiveEdge
import Tokens._


object RelationshipEdgeFactory {

  def makeSpouseFactory(gender: Item): EdgeFactory =
    (node, env) =>
      out(P.hasGender, gender)
          .and(in(node, P.hasSpouse))

  def makeChildFactory(gender: Item): EdgeFactory =
    (node, env) =>
      out(P.hasGender, gender)
          .and(in(node, P.hasChild))

  val factories: Map[String, EdgeFactory] =
    Map(// NOTE: weaker form, not P.isA, Q.president
      "president" -> reverse(P.hasHeadOfState),
      "actor" -> { (node, env) =>
        out(P.hasOccupation, Q.actor)
            .and(in(node, P.hasCastMember))
      },
      "album" -> { (node, env) =>
        out(P.isA, Q.album)
            .and(out(P.hasPerformer, node))
      },
      "director" -> { (node, env) =>
        out(P.isA, Q.filmDirector)
            .and(in(node, P.hasDirector))
      },
      "member" -> P.isMemberOf,
      "music genre" -> { (node, env) =>
        out(P.isA, Q.musicGenre)
            .and(in(node, P.hasGenre))
      },
      "cast" -> reverse(P.hasCastMember),
      "sister" -> reverse(P.hasSister),
      "child" -> reverse(P.hasChild),
      "population size" -> reverse(P.hasPopulation),
      "population" -> reverse(P.hasPopulation),
      "daughter" -> makeChildFactory(Q.female),
      "son" -> makeChildFactory(Q.male),
      "city" -> { (node, env) =>
        out(P.isA, Q.city)
            .and(out(P.isLocatedIn, node))
      },
      "husband" -> makeSpouseFactory(Q.male),
      "wife" -> makeSpouseFactory(Q.female),
      "spouse" -> reverse(P.hasSpouse),
      "grandfather" -> {
        (node, env) => {
          val child = env.newNode()
              .out(P.hasChild, node)
          out(P.hasGender, Q.male)
              .and(out(P.hasChild, child))
        }
      },
      "grandchild" -> {
        (node, env) => {
          val parent = env.newNode()
              .in(node, P.hasChild)
          in(parent, P.hasChild)
        }
      },
      "area" -> reverse(P.hasArea),
      "land area" -> reverse(P.hasArea),
      "capital" -> reverse(P.hasCapital),
      "discoverer" -> reverse(P.hasDiscovererOrInventor),
      "inventor" -> reverse(P.hasDiscovererOrInventor))
}


trait RelationshipEdgeFactory {

  def makeRelationshipEdge(name: Seq[Token], node: WikidataNode,
                           env: WikidataEnvironment): WikidataEdge =
  {
    import RelationshipEdgeFactory._

    val (adjectives, nameRest) = splitName(name)
    val lemmatized = mkLemmaString(nameRest)
    factories.get(lemmatized) map { factory =>
      val edge = factory(node, env)
      makeAdjectiveEdge(adjectives, lemmatized, env)
          .map(edge.and)
          .getOrElse(edge)
    } getOrElse {
      throw new RuntimeException(s"No relationship edge factory for '$lemmatized' ($name)")
    }
  }

}
