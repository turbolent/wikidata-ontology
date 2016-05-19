package com.turbolent.wikidataOntology

import com.turbolent.questionParser.Token


object Tokens {

  def mkLemmaString(tokens: Seq[Token]) =
    tokens.map(_.lemma).mkString(" ")

  def mkWordString(name: Seq[Token]) =
    name.map(_.word).mkString(" ")

  def stripInitialDeterminer(name: Seq[Token]) =
    name match {
      case Token(_, "DT", _) :: rest => rest
      case _ => name
    }

  def splitName(name: Seq[Token]) =
    stripInitialDeterminer(name)
        .span(_.pennTag.startsWith("JJ"))

}
