package com.turbolent.wikidataOntology

import com.turbolent.questionCompiler.EdgeContext
import Tokens._
import scala.collection.mutable


object HaveEdgeFactory {

  val factories: mutable.Map[String, ContextfulEdgeFactory] =
    mutable.Map(
      "child" -> P.hasChild,
      "inhabitant" -> P.hasPopulation)

  def makeHaveEdge(node: WikidataNode, context: EdgeContext,
                   env: WikidataEnvironment): WikidataEdge =
  {
    val key =
      if (context.valueIsNumber)
      // TODO: unit might be empty
        mkLemmaString(context.unit)
      else
        mkLemmaString(context.value)
    factories.get(key) map {
      _(node, context, env)
    } getOrElse {
      val message = s"No 'have' property edge factory for context: $context"
      throw new RuntimeException(message)
    }
  }

}
