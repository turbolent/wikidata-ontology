package com.turbolent.wikidataOntology

import com.turbolent.questionParser.Token


object Tokens {

  // TODO: other combinations?
  def mkLemmaString(tokens: Seq[Token]) =
    tokens.map { token =>
      val lemmas = token.lemmas
      if (lemmas.isEmpty)
        token.word
      else
        lemmas.head
    }.mkString(" ")

  def mkWordString(name: Seq[Token]) =
    name.map(_.word).mkString(" ")

  def stripInitialDeterminer(name: Seq[Token]) =
    name match {
      case Token(_, "DT") :: rest => rest
      case _ => name
    }

  def splitName(name: Seq[Token]) =
    stripInitialDeterminer(name)
        .span(_.pennTag.startsWith("JJ"))

}
