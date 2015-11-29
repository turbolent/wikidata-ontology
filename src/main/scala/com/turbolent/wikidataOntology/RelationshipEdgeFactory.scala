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

  def makeParentFactory(gender: Item): EdgeFactory =
    (node, env) =>
      out(P.hasGender, gender)
          .and(out(P.hasChild, node))

  def makeGrandparentFactory(gender: Option[Item]): EdgeFactory =
    (node, env) => {
      val child = env.newNode()
          .out(P.hasChild, node)
      val edge = out(P.hasChild, child)
      gender map { gender =>
        edge.and(out(P.hasGender, gender))
      } getOrElse edge
    }

  def makeGrandchildFactory(gender: Option[Item]): EdgeFactory =
    (node, env) => {
      val parent = env.newNode()
          .in(node, P.hasChild)
      val edge = in(parent, P.hasChild)
      gender map { gender =>
        edge.and(out(P.hasGender, gender))
      } getOrElse edge
    }

  def makeSpouseParentFactory(gender: Item): EdgeFactory =
    (node, env) => {
      val child = env.newNode()
          .out(P.hasSpouse, node)
      out(P.hasGender, gender)
          .and(out(P.hasChild, child))
    }

  val factories: Map[String, EdgeFactory] =
    Map(
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
      "population size" -> reverse(P.hasPopulation),
      "population" -> reverse(P.hasPopulation),
      "sister" -> reverse(P.hasSister),
      "brother" -> reverse(P.hasBrother),
      "child" -> reverse(P.hasChild),
      "parent" -> P.hasChild,
      "daughter" -> makeChildFactory(Q.female),
      "son" -> makeChildFactory(Q.male),
      "mother" -> makeParentFactory(Q.female),
      "father" -> makeParentFactory(Q.male),
      "husband" -> makeSpouseFactory(Q.male),
      "wife" -> makeSpouseFactory(Q.female),
      "spouse" -> reverse(P.hasSpouse),
      "grandparent" -> makeGrandparentFactory(None),
      "grandmother" -> makeGrandparentFactory(Some(Q.female)),
      "grandfather" -> makeGrandparentFactory(Some(Q.male)),
      "grandchild" -> makeGrandchildFactory(None),
      "granddaughter" -> makeGrandchildFactory(Some(Q.female)),
      "grandson" -> makeGrandchildFactory(Some(Q.male)),
      "father-in-law" -> makeSpouseParentFactory(Q.male),
      "mother-in-law" -> makeSpouseParentFactory(Q.female),
      "movie" -> { (node, env) =>
        out(P.isA, Q.movie)
            .and(out(P.hasDirector, node))
      },
      "city" -> { (node, env) =>
        out(P.isA, Q.city)
            .and(out(P.isLocatedIn, node))
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
