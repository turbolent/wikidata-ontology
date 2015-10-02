package com.turbolent.wikidataOntology


case class Item(id: Int, name: String)

package object Q {
  val human = Item(5, "human")
  val city = Item(515, "city")
  val book = Item(571, "book")
  val mountain = Item(8502, "mountain")
  val movie = Item(11424, "movie")
  val president = Item(30461, "president")
  val actor = Item(33999, "actor")
  val musicalInstrument = Item(34379, "musical instrument")
  val language = Item(34770, "language")
  val musicGenre = Item(189991, "music genre")
  val album = Item(482994, "album")
  val artist = Item(483501, "artist")
  val musician = Item(639669, "musician")
  val filmDirector = Item(2526255, "film director")
  val male = Item(6581097, "male")
  val female = Item(6581072, "female")
  val planet = Item(634, "planet")
}