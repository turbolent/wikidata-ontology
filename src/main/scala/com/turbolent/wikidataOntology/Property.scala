package com.turbolent.wikidataOntology

case class Property(id: Int, name: String)

package object P {
  val hasBrother = Property(7, "has brother")
  val hasSister = Property(9, "has sister")
  val hasPlaceOfBirth = Property(19, "has place of birth")
  val hasPlaceOfDeath = Property(20, "has place of death")
  val hasGender = Property(21, "has gender")
  val hasFather = Property(22, "has father")
  val hasMother = Property(25, "has mother")
  val hasSpouse = Property(26, "has spouse")
  // NOTE: avoid name clash with Scala's isInstanceOf
  val isA = Property(31, "is instance of")
  val hasHeadOfState = Property(35, "has head of state")
  val hasCapital = Property(36, "has capital")
  val hasOfficialLanguage = Property(37, "has official language")
  val hasChild = Property(40, "has child")
  val hasAuthor = Property(50, "has author")
  val hasDirector = Property(57, "has director")
  val hasOccupation = Property(106, "has occupation")
  val isLocatedIn = Property(131, "is located in")
  val hasGenre = Property(136, "has genre")
  val hasCastMember = Property(161, "has cast member")
  val hasPerformer = Property(175, "has performer")
  val isMemberOf = Property(463, "is member of")
  val hasDateOfBirth = Property(569, "has date of birth")
  val hasDateOfDeath = Property(570, "has date of death")
  val hasFilmingLocation = Property(915, "has filming location")
  val hasRelative = Property(1038, "has relative")
  val hasPopulation = Property(1082, "has population")
  val playsInstrument = Property(1303, "plays instrument")
  val hasElevation = Property(2044, "has elevation above sea level")
  val hasArea = Property(2046, "has area")
  val hasDiscovererOrInventor = Property(61, "has discoverer or inventor")
}

