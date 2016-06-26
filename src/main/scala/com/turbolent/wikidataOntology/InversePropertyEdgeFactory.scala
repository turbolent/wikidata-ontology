package com.turbolent.wikidataOntology

import com.turbolent.questionCompiler.{NamedSubject, EdgeContext}
import com.turbolent.questionParser.{ListParser, Token}
import Tokens._
import scala.collection.mutable


object InversePropertyEdgeFactory {

  val factories: mutable.Map[String, ContextfulEdgeFactory] =
    mutable.Map(
      "appear" -> P.hasCastMember,
      "marry" -> P.hasSpouse,
      "write" -> P.hasAuthor,
      "direct" -> P.hasDirector,
      "play" -> contextfulReverse(P.playsInstrument),
      "bear in" -> { (node, context, env) =>
        context.subject match {
          case NamedSubject(subject) =>
            val lemmatizedSubject =
              mkLemmaString(stripInitialAuxiliaryVerb(subject))
            lemmatizedSubject match {
              case "country" =>
                val place = env.newNode()
                    .in(node, P.hasPlaceOfBirth)
                in(place, P.country)
              case "year" =>
                val date = env.newNode()
                    .in(node, P.hasDateOfBirth)
                in(date, YearLabel)
            }
          case _ => ???
        }
      },
      "attend" -> contextfulReverse(P.wasEducatedAt),
      "study" -> contextfulReverse(P.wasEducatedAt))

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
    factories.get(lemmatized) map {
      _(node, context, env)
    } getOrElse {
      throw new RuntimeException(s"No inverse property edge factory for '$lemmatized' " +
                                 s"(${name.mkString(", ")}), context: $context")
    }
  }

}
