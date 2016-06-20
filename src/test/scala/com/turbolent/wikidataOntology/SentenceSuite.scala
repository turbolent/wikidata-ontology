package com.turbolent.wikidataOntology

import java.time.Year

import com.turbolent.questionCompiler.graph._
import org.scalatest.FunSuite


// scalastyle:off multiple.string.literals

class SentenceSuite extends FunSuite with Utilities {

  def test(sentence: String,
           expectedNodesGenerator: (WikidataEnvironment) => List[WikidataNode],
           expectedQueries: (String, String, Option[String])*) {

    val tokens = tokenize(sentence)
    val testName = tokens.map(_.word).mkString(" ")

    test(testName) {
      val actualNodes = compileListQuestion(tokens)
      val env = new WikidataEnvironment()
      val expectedNodes = expectedNodesGenerator(env)
      actualNodes shouldEqual expectedNodes

      for ((expectedNode, (variable, expectedQueryString, ordering)) <- expectedNodes zip expectedQueries) {
        val actualQuery = compileSparqlQuery(expectedNode, env)
        val expectedQuery = parseSparqlQuery(variable, expectedQueryString, ordering)
        assertEquivalent(expectedQuery, actualQuery)
      }
    }
  }


  test("who/WP/who acted/VBD/act in/IN/in Alien/NNP/alien",

    // PersonListQuestion(PropertyWithFilter(List(Token("acted", "VBD", "act")),
    //   FilterWithModifier(List(Token("in", "IN", "in")), NamedValue(List(Token("Alien", "NNP", "alien"))))))

    { env =>
      val person = env.newNode()
        .out(P.isA, Q.human)

      val movie = env.newNode()
        .out(NameLabel, "Alien")

      List(person.in(movie, P.hasCastMember))
    },
    ("1", 
      """
        |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q5
        |  { ?2  wdt:P161    ?1 ;
        |        rdfs:label  "Alien"@en
        |  }
        |}
      """.stripMargin, 
      None))


  test("who/WP/who starred/VBD/star in/IN/in Inception/NNP/inception",

    // PersonListQuestion(PropertyWithFilter(List(Token("starred", "VBD", "star")),
    //   FilterWithModifier(List(Token("in", "IN", "in")),
    //     NamedValue(List(Token("Inception", "NNP", "inception")))))))

    { env =>
      val person = env.newNode()
        .out(P.isA, Q.human)

      val movie = env.newNode()
        .out(NameLabel, "Inception")

      List(person.in(movie, P.hasCastMember))
    },
    ("1",
      """
        |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q5
        |  { ?2  wdt:P161    ?1 ;
        |       rdfs:label  "Inception"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("Movies/NNP/movie starring/VB/star Winona/NNP/winona Ryder/NNP/ryder",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("Movies", "NNP", "movie"))),
    //   PropertyWithFilter(List(Token("starring", "VB", "star")),
    //     PlainFilter(NamedValue(List(Token("Winona", "NNP", "winona"), Token("Ryder", "NNP", "ryder")))))))

    { env =>
      val movie = env.newNode().out(P.isA, Q.movie)

      val actress = env.newNode()
          .out(NameLabel, "Winona Ryder")

      List(movie.out(P.hasCastMember, actress))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q11424
        |  { ?1  wdt:P161    ?2 .
        |    ?2  rdfs:label  "Winona Ryder"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("In/IN/in what/WDT/what movies/NN/movie did/VBD/do Jennifer/NNP/jennifer Aniston/NNP/aniston appear/VB/appear",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("movies", "NN", "movie"))),
    //   InversePropertyWithFilter(List(Token("did", "VBD", "do"), Token("appear", "VB", "appear")),
    //     PlainFilter(NamedValue(List(Token("Jennifer", "NNP", "jennifer"), Token("Aniston", "NNP", "aniston")))))))

    { env =>
      val movie = env.newNode().out(P.isA, Q.movie)

      val actress = env.newNode()
          .out(NameLabel, "Jennifer Aniston")

      List(movie.out(P.hasCastMember, actress))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q11424
        |  { ?1  wdt:P161    ?2 .
        |    ?2  rdfs:label  "Jennifer Aniston"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("who/WP/who are/VBP/be the/DT/the actors/NNS/actor "
      + "which/WDT/which starred/VBD/star in/IN/in Inception/NNP/inception",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("the", "DT", "the"),
    //   Token("actors", "NNS", "actor"))),
    //     PropertyWithFilter(List(Token("starred", "VBD", "star")),
    //       FilterWithModifier(List(Token("in", "IN", "in")),
    //         NamedValue(List(Token("Inception", "NNP", "inception")))))))

    { env =>
      val person = env.newNode()
        .out(P.isA, Q.actor)
        .or(out(P.hasOccupation, Q.actor))

      val movie = env.newNode()
        .out(NameLabel, "Inception")

      List(person.in(movie, P.hasCastMember))
    },
    ("1",
      """
        |{   { ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q33999 }
        |  UNION
        |    { ?1 p:P106/(v:P106/(wdt:P279)*) wd:Q33999 }
        |  ?2  wdt:P161    ?1 ;
        |      rdfs:label  "Inception"@en
        |}
      """.stripMargin,
      None))


  test("who/WP/who directed/VBD/direct Pocahontas/NNP/pocahontas",

    // PersonListQuestion(PropertyWithFilter(List(Token("directed", "VBD", "direct")),
    //   PlainFilter(NamedValue(List(Token("Pocahontas", "NNP", "pocahontas"))))))

    { env =>
      val person = env.newNode()
          .out(P.isA, Q.human)

      val movie = env.newNode()
          .out(NameLabel, "Pocahontas")

      List(person.in(movie, P.hasDirector))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
        |  { ?2  wdt:P57     ?1 ;
        |        rdfs:label  "Pocahontas"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("Who/WP/who did/VBD/do Bill/NNP/bill Clinton/NNP/clinton marry/VB/marry",

    // PersonListQuestion(InversePropertyWithFilter(List(Token("did", "VBD", "do"),
    //   Token("marry", "VB", "marry")),
    //   PlainFilter(NamedValue(List(Token("Bill", "NNP", "bill"), Token("Clinton", "NNP", "clinton"))))))

    { env =>
      val person = env.newNode()
          .out(P.isA, Q.human)

      val bill = env.newNode()
          .out(NameLabel, "Bill Clinton")

      List(person.out(P.hasSpouse, bill))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
        |  { ?1  wdt:P26     ?2 .
        |    ?2  rdfs:label  "Bill Clinton"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("which/WDT/which mountains/NNS/mountain are/VBP/be 1000/CD/1000 meters/NNS/meter high/JJ/high",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("mountains", "NNS", "mountain"))),
    //   AdjectivePropertyWithFilter(List(Token("are", "VBP", "be"), Token("high", "JJ", "high")),
    //     PlainFilter(NumberWithUnit(List(Token("1000", "CD", "1000")), List(Token("meters", "NNS", "meter")))))))

    { env =>
      val elevation: WikidataNode = (1000.0, U.meter)

      val mountain = env.newNode()
          .out(P.isA, Q.mountain)
          .out(P.hasElevation, elevation)

      List(mountain)
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q8502
        |  { ?1  wdt:P2044  "1000.0"^^xsd:integer }
        |}
      """.stripMargin,
      None))


  test("authors/NNS/author which/WDT/which died/VBD/die in/IN/in Berlin/NNP/berlin",

    //    ListQuestion(QueryWithProperty(NamedQuery(List(Token("authors", "NNS", "author"))),
    //      PropertyWithFilter(List(Token("died", "VBD", "die")),
    //        FilterWithModifier(List(Token("in", "IN", "in")),
    //          NamedValue(List(Token("Berlin", "NNP", "berlin")))))))

    { env =>
      val author = env.newNode()
          .in(env.newNode(), P.hasAuthor)

      val place = env.newNode()
          .out(NameLabel, "Berlin")

      List(author.out(P.hasPlaceOfDeath, place))
    },
    ("1",
      """
        |{ ?2  wdt:P50  ?1
        |  { ?1 wdt:P20/(wdt:P131)* ?3
        |    { ?3  rdfs:label  "Berlin"@en }
        |  }
        |}
      """.stripMargin,
      None))


  test("authors/NNS/author which/WDT/which were/VBD/be born/VBD/bear in/IN/in Berlin/NNP/berlin"
    + " and/CC/and died/VBD/die in/IN/in Paris/NNP/paris",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("authors", "NNS", "author"))),
    //   AndProperty(List(PropertyWithFilter(List(Token("were", "VBD", "be"),
    //     Token("born", "VBD", "bear")), FilterWithModifier(List(Token("in", "IN", "in")),
    //     NamedValue(List(Token("Berlin", "NNP", "berlin"))))),
    //     PropertyWithFilter(List(Token("died", "VBD", "die")),
    //       FilterWithModifier(List(Token("in", "IN", "in")),
    //         NamedValue(List(Token("Paris", "NNP", "paris")))))))))

    { env =>
      val author = env.newNode()
          .in(env.newNode(), P.hasAuthor)

      val place = env.newNode()
          .out(NameLabel, "Berlin")

      val place2 = env.newNode()
          .out(NameLabel, "Paris")

      List(author
          .out(P.hasPlaceOfBirth, place)
          .out(P.hasPlaceOfDeath, place2))
    },
    ("1",
      """
        |{ { ?2  wdt:P50  ?1
        |    { ?1 wdt:P19/(wdt:P131)* ?3
        |      { ?3  rdfs:label  "Berlin"@en }
        |    }
        |  }
        |  { ?1 wdt:P20/(wdt:P131)* ?4
        |    { ?4  rdfs:label  "Paris"@en }
        |  }
        |}
      """.stripMargin,
      None))


  test("who/WP/who was/VBD/be born/VBD/bear in/IN/in Berlin/NNP/berlin or/CC/or died/VBD/die in/IN/in Paris/NNP/paris",

    // PersonListQuestion(OrProperty(List(PropertyWithFilter(List(Token("was", "VBD", "be"),
    //   Token("born", "VBD", "bear")), FilterWithModifier(List(Token("in", "IN", "in")),
    //   NamedValue(List(Token("Berlin", "NNP", "berlin"))))),
    //   PropertyWithFilter(List(Token("died", "VBD", "die")),
    //     FilterWithModifier(List(Token("in", "IN", "in")),
    //       NamedValue(List(Token("Paris", "NNP", "paris"))))))))

    { env =>
      val author = env.newNode()
          .out(P.isA, Q.human)

      val place = env.newNode()
          .out(NameLabel, "Berlin")

      val place2 = env.newNode()
          .out(NameLabel, "Paris")

      List(author
          .and(out(P.hasPlaceOfBirth, place)
               or out(P.hasPlaceOfDeath, place2)))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
        |    { ?1 wdt:P19/(wdt:P131)* ?2
        |      { ?2  rdfs:label  "Berlin"@en }
        |    }
        |  UNION
        |    { ?1 wdt:P20/(wdt:P131)* ?3
        |      { ?3  rdfs:label  "Paris"@en }
        |    }
        |}
      """.stripMargin,
      None))


  test("Which/WDT/which presidents/NNS/president were/VBD/be born/VBN/bear before/IN/before 1900/CD/1900",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("presidents", "NNS", "president"))),
    //   PropertyWithFilter(List(Token("were", "VBD", "be"), Token("born", "VBN", "bear")),
    //     FilterWithModifier(List(Token("before", "IN", "before")), Number(List(Token("1900", "CD", "1900")))))))

    { env =>
      val president = env.newNode()
          .out(P.holdsPosition, Q.president)

      val year: WikidataNode = Year.of(1900)

      val date = env.newNode()
          .filter(LessThanFilter(year))

      List(president.out(P.hasDateOfBirth, date))
    },
    ("1",
      """
        |{ ?1 p:P39/(v:P39/(wdt:P279)*) wd:Q30461
        |  { ?1  wdt:P569  ?2
        |    FILTER ( year(?2) < 1900 )
        |  }
        |}
      """.stripMargin,
      None))


  test("Give/VB/give me/PRP/me all/DT/all actors/NNS/actor born/VBN/bear"
    + " in/IN/in Berlin/NNP/berlin or/CC/or San/NNP/san Francisco/NNP/francisco",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("actors", "NNS", "actor"))),
    //   PropertyWithFilter(List(Token("born", "VBN", "bear")), FilterWithModifier(List(Token("in", "IN", "in")),
    //     OrValue(List(NamedValue(List(Token("Berlin", "NNP", "berlin"))),
    //       NamedValue(List(Token("San", "NNP", "san"), Token("Francisco", "NNP", "francisco")))))))))

    { env =>
      val actor = env.newNode()
          .out(P.isA, Q.actor)
          .or(out(P.hasOccupation, Q.actor))

      val place = env.newNode()
          .out(NameLabel, "Berlin")

      val place2 = env.newNode()
          .out(NameLabel, "San Francisco")

      List(actor
          .and(out(P.hasPlaceOfBirth, place)
               or out(P.hasPlaceOfBirth, place2)))
    },
    ("1",
      """
        |{   { ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q33999 }
        |  UNION
        |    { ?1 p:P106/(v:P106/(wdt:P279)*) wd:Q33999 }
        |
        |    { ?1 wdt:P19/(wdt:P131)* ?2
        |      { ?2  rdfs:label  "Berlin"@en }
        |    }
        |  UNION
        |    { ?1 wdt:P19/(wdt:P131)* ?3
        |      { ?3  rdfs:label  "San Francisco"@en }
        |    }
        |}
      """.stripMargin,
      None))


  test("Which/WDT/which movies/NNS/movie "
      + "were/VBD/be filmed/VBD/film in/IN/in Germany/NNP/germany "
      + "and/CC/and Denmark/NNP/denmark or/CC/or California/NNP/california",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("movies", "NNS", "movie"))),
    //   PropertyWithFilter(List(Token("were", "VBD", "be"), Token("filmed", "VBD", "film")),
    //     FilterWithModifier(List(Token("in", "IN", "in")),
    //       OrValue(List(AndValue(List(NamedValue(List(Token("Germany", "NNP", "germany"))),
    //         NamedValue(List(Token("Denmark", "NNP", "denmark"))))),
    //         NamedValue(List(Token("California", "NNP", "california")))))))))

    { env =>
      val movie = env.newNode()
          .out(P.isA, Q.movie)

      val germany = env.newNode()
          .out(NameLabel, "Germany")

      val denmark = env.newNode()
          .out(NameLabel, "Denmark")

      val california = env.newNode()
          .out(NameLabel, "California")

      List(movie
          .and((out(P.hasFilmingLocation, germany) and
                out(P.hasFilmingLocation, denmark))
               or out(P.hasFilmingLocation, california)))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q11424
        |    { { ?1 wdt:P915/(wdt:P131)* ?2
        |        { ?2  rdfs:label  "Germany"@en }
        |      }
        |      { ?1 wdt:P915/(wdt:P131)* ?3
        |        { ?3  rdfs:label  "Denmark"@en }
        |      }
        |    }
        |  UNION
        |    { ?1 wdt:P915/(wdt:P131)* ?4
        |      { ?4  rdfs:label  "California"@en }
        |    }
        |}
      """.stripMargin,
      None))


  test("Give/VB/give me/PRP/me all/DT/all musicians/NNS/musician that/WDT/that were/VBD/be born/VBN/bear"
    + " in/IN/in Vienna/NNP/vienna and/CC/and died/VBN/die in/IN/in Berlin/NNP/berlin",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("musicians", "NNS", "musician"))),
    //   AndProperty(List(PropertyWithFilter(List(Token("were", "VBD", "be"), Token("born", "VBN", "bear")),
    //     FilterWithModifier(List(Token("in", "IN", "in")),
    //       NamedValue(List(Token("Vienna", "NNP", "vienna"))))),
    //     PropertyWithFilter(List(Token("died", "VBN", "die")),
    //       FilterWithModifier(List(Token("in", "IN", "in")),
    //         NamedValue(List(Token("Berlin", "NNP", "berlin")))))))))

    { env =>
      val musician = env.newNode()
          .out(P.isA, Q.musician)
          .or(out(P.hasOccupation, Q.musician))

      val place = env.newNode()
          .out(NameLabel, "Vienna")

      val place2 = env.newNode()
          .out(NameLabel, "Berlin")

      List(musician
          .out(P.hasPlaceOfBirth, place)
          .out(P.hasPlaceOfDeath, place2))
    },
    ("1",
      """
        |{ {   { ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q639669 }
        |    UNION
        |      { ?1 p:P106/(v:P106/(wdt:P279)*) wd:Q639669 }
        |
        |    { ?1 wdt:P19/(wdt:P131)* ?2
        |      { ?2  rdfs:label  "Vienna"@en }
        |    }
        |  }
        |  { ?1 wdt:P20/(wdt:P131)* ?3
        |    { ?3  rdfs:label  "Berlin"@en }
        |  }
        |}
      """.stripMargin,
      None))


  test("Which/WDT/which books/NN/book did/VBD/do George/NNP/george Orwell/NNP/orwell write/VB/write",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NN", "book"))),
    //   InversePropertyWithFilter(List(Token("did", "VBD", "do"), Token("write", "VB", "write")),
    //     PlainFilter(NamedValue(List(Token("George", "NNP", "george"), Token("Orwell", "NNP", "orwell")))))))

    { env =>
      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      List(book.out(P.hasAuthor, author))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q571
        |  { ?1  wdt:P50     ?2 .
        |    ?2  rdfs:label  "George Orwell"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("list/VB/list presidents/NNS/president of/IN/of Argentina/NNP/argentina",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("presidents", "NNS", "president"))),
    //   NamedQuery(List(Token("Argentina", "NNP", "argentina"))), Token("of", "IN", "of")))

    { env =>
      val country = env.newNode()
          .out(NameLabel, "Argentina")

      val person = env.newNode()
          .in(country, P.hasHeadOfState)

      List(person)
    },
    ("2",
      """
        |{ ?1  wdt:P35     ?2 ;
        |      rdfs:label  "Argentina"@en
        |}
      """.stripMargin,
      None))


  test("List/VB/list albums/NNS/album of/IN/of Pink/NNP/pink Floyd/NNP/floyd",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("albums", "NNS", "album"))),
    //   NamedQuery(List(Token("Pink", "NNP", "pink"), Token("Floyd", "NNP", "floyd"))), Token("of", "IN", "of")))

    { env =>
      val artist = env.newNode()
          .out(NameLabel, "Pink Floyd")

      val album = env.newNode()
          .out(P.isA, Q.album)
          .out(P.hasPerformer, artist)

      List(album)
    },
    ("2",
      """
        |{ ?2 p:P31/(v:P31/(wdt:P279)*) wd:Q482994
        |  { ?2  wdt:P175    ?1 .
        |    ?1  rdfs:label  "Pink Floyd"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("List/VB/list the/DT/the actors/NNS/actor of/IN/of Titanic/NNP/titanic",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
    //   Token("actors", "NNS", "actor"))),
    //   NamedQuery(List(Token("Titanic", "NNP", "titanic"))), Token("of", "IN", "of")))

    { env =>
      val movie = env.newNode()
          .out(NameLabel, "Titanic")

      val actor = env.newNode()
          .out(P.hasOccupation, Q.actor)
          .in(movie, P.hasCastMember)

      List(actor)
    },
    ("2",
      """
        |{ ?2 p:P106/(v:P106/(wdt:P279)*) wd:Q33999
        |  { ?1  wdt:P161    ?2 ;
        |        rdfs:label  "Titanic"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("Who/WP/who is/VBZ/be the/DT/the director/NN/director of/IN/of Big/NN/big Fish/NN/fish",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
    //   Token("director", "NN", "director"))),
    //   NamedQuery(List(Token("Big", "NN", "big"), Token("Fish", "NN", "fish"))), Token("of", "IN", "of")))

    { env =>
      val movie = env.newNode()
          .out(NameLabel, "Big Fish")

      val director = env.newNode()
          .out(P.isA, Q.filmDirector)
          .in(movie, P.hasDirector)

      List(director)
    },
    ("2",
      """
        |{ ?2 p:P31/(v:P31/(wdt:P279)*) wd:Q2526255
        |  { ?1  wdt:P57     ?2 ;
        |        rdfs:label  "Big Fish"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("What/WP/what are/VBP/be the/DT/the members/NNS/member of/IN/of Metallica/NNP/metallica",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
    //     Token("members", "NNS", "member"))),
    //   NamedQuery(List(Token("Metallica", "NNP", "metallica"))), Token("of", "IN", "of")))

    { env =>
      val band = env.newNode()
          .out(NameLabel, "Metallica")

      val member = env.newNode()
          .out(P.isMemberOf, band)

      List(member)
    },
    ("2",
      """
        |{ ?2  wdt:P463    ?1 .
        |  ?1  rdfs:label  "Metallica"@en
        |}
      """.stripMargin,
      None))


  test("members/NNS/member of/IN/of Metallica/NNP/metallica",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("members", "NNS", "member"))),
    //   NamedQuery(List(Token("Metallica", "NNP", "metallica"))), Token("of", "IN", "of")))

    { env =>
      val band = env.newNode()
          .out(NameLabel, "Metallica")

      val member = env.newNode()
          .out(P.isMemberOf, band)

      List(member)
    },
    ("2",
      """
        |{ ?2  wdt:P463    ?1 .
        |  ?1  rdfs:label  "Metallica"@en
        |}
      """.stripMargin,
      None))


  test("What/WP/what is/VBZ/be the/DT/the music/NN/music genre/NN/genre of/IN/of Gorillaz/NNP/gorillaz",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
    //     Token("music", "NN", "music"), Token("genre", "NN", "genre"))),
    //   NamedQuery(List(Token("Gorillaz", "NNP", "gorillaz"))), Token("of", "IN", "of")))

    { env =>
      val band = env.newNode()
          .out(NameLabel, "Gorillaz")

      val genre = env.newNode()
          .out(P.isA, Q.musicGenre)
          .in(band, P.hasGenre)

      List(genre)
    },
    ("2",
      """
        |{ ?2 p:P31/(v:P31/(wdt:P279)*) wd:Q189991
        |  { ?1  wdt:P136    ?2 ;
        |        rdfs:label  "Gorillaz"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("What/WP/what is/VBZ/be the/DT/the cast/NN/cast of/IN/of Friends/NNS/friend",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"), Token("cast", "NN", "cast"))),
    //   NamedQuery(List(Token("Friends", "NNS", "friend"))), Token("of", "IN", "of")))

    { env =>
      val show = env.newNode()
          .out(NameLabel, "Friends")

      val member = env.newNode()
          .in(show, P.hasCastMember)

      List(member)
    },
    ("2",
      """
        |{ ?1  wdt:P161    ?2 ;
        |      rdfs:label  "Friends"@en
        |}
      """.stripMargin,
      None))


  test("who/WP/who are/VBP/be the/DT/the children/NNS/child of/IN/of the/DT/the presidents/NNS/president",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
    //     Token("children", "NNS", "child"))),
    //   NamedQuery(List(Token("the", "DT", "the"), Token("presidents", "NNS", "president"))),
    //     Token("of", "IN", "of")))

    { env =>
      val president = env.newNode()
          .out(P.holdsPosition, Q.president)

      val child = env.newNode()
          .in(president, P.hasChild)

      List(child)
    },
    ("2",
      """
        |{ ?1  wdt:P40  ?2
        |  { ?1 p:P39/(v:P39/(wdt:P279)*) wd:Q30461 }
        |}
      """.stripMargin,
      None))


  test("what/WP/what are/VBP/be the/DT/the population/NN/population sizes/NNS/size"
      + " of/IN/of cities/NNS/city located/VBN/locate in/IN/in california/NN/california",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
    //   Token("population", "NN", "population"),
    //   Token("sizes", "NNS", "size"))),
    //   QueryWithProperty(NamedQuery(List(Token("cities", "NNS", "city"))),
    //     PropertyWithFilter(List(Token("located", "VBN", "locate")),
    //       FilterWithModifier(List(Token("in", "IN", "in")),
    //         NamedValue(List(Token("california", "NN", "california")))))),
    //   Token("of", "IN", "of")))

    { env =>
      val city = env.newNode()
          .out(P.isA, Q.city)

      val california = env.newNode()
          .out(NameLabel, "california")

      val californianCity = city
          .and(out(P.isLocatedIn, california)
               or out(P.country, california)
               or out(P.hasHeadquartersLocation, california))

      val population = env.newNode()
          .in(californianCity, P.hasPopulation)

      List(population)
    },
    ("3",
      """
        |{ ?1  wdt:P1082  ?3
        |  { ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q515
        |      {   { ?1 (wdt:P131)+ ?2
        |            { ?2  rdfs:label  "california"@en }
        |          }
        |        UNION
        |          { ?1  wdt:P17     ?2 .
        |            ?2  rdfs:label  "california"@en
        |          }
        |      }
        |    UNION
        |      { ?1 wdt:P159/(wdt:P131)* ?2
        |        { ?2  rdfs:label  "california"@en }
        |      }
        |  }
        |}
      """.stripMargin,
      None))


  test("Who/WP/who are/VBP/be the/DT/the children/NNS/child of/IN/of the/DT/the children/NNS/child"
    + " of/IN/of Bill/NNP/bill Clinton/NNP/clinton",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
    //   Token("children", "NNS", "child"))),
    //   RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"), Token("children", "NNS", "child"))),
    //     NamedQuery(List(Token("Bill", "NNP", "bill"), Token("Clinton", "NNP", "clinton"))),
    //     Token("of", "IN", "of")),
    //   Token("of", "IN", "of")))

    { env =>
      val bill = env.newNode()
          .out(NameLabel, "Bill Clinton")

      val child = env.newNode()
          .in(bill, P.hasChild)

      val grandChild = env.newNode()
          .in(child, P.hasChild)

      List(grandChild)
    },
    ("3",
      """
        |{ ?2  wdt:P40     ?3 .
        |  ?1  wdt:P40     ?2 ;
        |      rdfs:label  "Bill Clinton"@en
        |}
      """.stripMargin,
      None))


  test("Clinton/NNP/clinton 's/POS/'s daughters/NN/daughter",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("daughters", "NN", "daughter"))),
    //     NamedQuery(List(Token("Clinton", "NNP", "clinton"))),
    //   Token("'s", "POS", "'s")))

    { env =>
      val bill = env.newNode()
          .out(NameLabel, "Clinton")

      val child = env.newNode()
          .out(P.hasGender, Q.female)
          .in(bill, P.hasChild)

      List(child)
    },
    ("2",
      """
        |{ ?2  wdt:P21     wd:Q6581072 .
        |  ?1  wdt:P40     ?2 ;
        |      rdfs:label  "Clinton"@en
        |}
      """.stripMargin,
      None))


  test("What/WP/what are/VBP/be the/DT/the largest/JJS/large cities/NNS/city of/IN/of California/NNP/california",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
    //     Token("largest", "JJS", "large"), Token("cities", "NNS", "city"))),
    //   NamedQuery(List(Token("California", "NNP", "california"))),
    //   Token("of", "IN", "of")))

    { env =>
      val state = env.newNode()
        .out(NameLabel, "California")

      val population = env.newNode()
        .order(Descending)

      val city = env.newNode()
        .out(P.isA, Q.city)
        .out(P.isLocatedIn, state)
        .out(P.hasPopulation, population)

      List(city)
    },
    ("3",
      """
        |{ { ?3 p:P31/(v:P31/(wdt:P279)*) wd:Q515
        |    { ?3 (wdt:P131)+ ?1
        |      { ?1  rdfs:label  "California"@en }
        |    }
        |  }
        |  ?3  wdt:P1082  ?2
        |}
      """.stripMargin,
      Some("DESC(?2)")))


  test("What/WP/what are/VBP/be the/DT/the biggest/JJS/big cities/NNS/city"
      + " of/IN/of California/NNP/california",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
    //   Token("biggest", "JJS", "big"), Token("cities", "NNS", "city"))),
    //   NamedQuery(List(Token("California", "NNP", "california"))),
    //   Token("of", "IN", "of")))

    { env =>
      val state = env.newNode()
          .out(NameLabel, "California")

      val population = env.newNode()
          .order(Descending)

      val city = env.newNode()
          .out(P.isA, Q.city)
          .out(P.isLocatedIn, state)
          .out(P.hasPopulation, population)

      List(city)
    },
    ("3",
      """
        |{ { ?3 p:P31/(v:P31/(wdt:P279)*) wd:Q515
        |    { ?3 (wdt:P131)+ ?1
        |      { ?1  rdfs:label  "California"@en }
        |    }
        |  }
        |  ?3  wdt:P1082  ?2
        |}
      """.stripMargin,
      Some("DESC(?2)")))


  test("What/WP/what are/VBP/be California/NNP/california 's/POS/'s"
    + " biggest/JJS/big cities/NNS/city",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("biggest", "JJS", "big"),
    //   Token("cities", "NNS", "city"))),
    //   NamedQuery(List(Token("California", "NNP", "california"))),
    //   Token("'s", "POS", "'s")))

    { env =>
      val state = env.newNode()
          .out(NameLabel, "California")

      val population = env.newNode()
          .order(Descending)

      val city = env.newNode()
          .out(P.isA, Q.city)
          .out(P.isLocatedIn, state)
          .out(P.hasPopulation, population)

      List(city)
    },
    ("3",
      """
        |{ { ?3 p:P31/(v:P31/(wdt:P279)*) wd:Q515
        |    { ?3 (wdt:P131)+ ?1
        |      { ?1  rdfs:label  "California"@en }
        |    }
        |  }
        |  ?3  wdt:P1082  ?2
        |}
      """.stripMargin,
      Some("DESC(?2)")))


  test("Who/WP/who is/VBZ/be Bill/NNP/bill Clinton/NNP/clinton 's/POS/'s daughter/NN/daughter",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("daughter", "NN", "daughter"))),
    //   NamedQuery(List(Token("Bill", "NNP", "bill"), Token("Clinton", "NNP", "clinton"))),
    //   Token("'s", "POS", "'s")))

    { env =>
      val bill = env.newNode()
          .out(NameLabel, "Bill Clinton")

      val daughter = env.newNode()
          .out(P.hasGender, Q.female)
          .in(bill, P.hasChild)

      List(daughter)
    },
    ("2",
      """
        |{ ?2  wdt:P21     wd:Q6581072 .
        |  ?1  wdt:P40     ?2 ;
        |      rdfs:label  "Bill Clinton"@en
        |}
      """.stripMargin,
      None))


  test("Who/WP/who is/VBZ/be Bill/NNP/bill Clinton/NNP/clinton 's/POS/'s daughter/NN/daughter 's/POS/'s"
    + " husband/NN/husband 's/POS/'s daughter/NN/daughter 's/POS/'s grandfather/NN/grandfather",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("grandfather", "NN", "grandfather"))),
    //   RelationshipQuery(NamedQuery(List(Token("daughter", "NN", "daughter"))),
    //     RelationshipQuery(NamedQuery(List(Token("husband", "NN", "husband"))),
    //       RelationshipQuery(NamedQuery(List(Token("daughter", "NN", "daughter"))),
    //         NamedQuery(List(Token("Bill", "NNP", "bill"), Token("Clinton", "NNP", "clinton"))),
    //         Token("'s", "POS", "'s")),
    //       Token("'s", "POS", "'s")),
    //     Token("'s", "POS", "'s")),
    //   Token("'s", "POS", "'s")))

    { env =>
      val bill = env.newNode()
          .out(NameLabel, "Bill Clinton")

      val daughter = env.newNode()
          .out(P.hasGender, Q.female)
          .in(bill, P.hasChild)

      val sonInLaw = env.newNode()
          .out(P.hasGender, Q.male)
          .in(daughter, P.hasSpouse)

      val grandDaughter = env.newNode()
          .out(P.hasGender, Q.female)
          .in(sonInLaw, P.hasChild)

      val parent = env.newNode()
          .out(P.hasChild, grandDaughter)

      val grandFather = env.newNode()
          .out(P.hasGender, Q.male)
          .out(P.hasChild, parent)

      List(grandFather)
    },
    ("6",
      """
        |{ ?6  wdt:P21     wd:Q6581097 ;
        |      wdt:P40     ?5 .
        |  ?5  wdt:P40     ?4 .
        |  ?4  wdt:P21     wd:Q6581072 .
        |  ?3  wdt:P40     ?4 ;
        |      wdt:P21     wd:Q6581097 .
        |  ?2  wdt:P26     ?3 ;
        |      wdt:P21     wd:Q6581072 .
        |  ?1  wdt:P40     ?2 ;
        |      rdfs:label  "Bill Clinton"@en
        |}
        |
      """.stripMargin,
      None))


  test("Who/WP/who are/VBP/be the/DT/the daughters/NNS/daughter of/IN/of the/DT/the wife/NN/wife of/IN/of"
    + " the/DT/the president/NN/president of/IN/of the/DT/the United/NNP/united States/NNPS/state",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
    //   Token("daughters", "NNS", "daughter"))),
    //   RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"), Token("wife", "NN", "wife"))),
    //     RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"), Token("president", "NN", "president"))),
    //       NamedQuery(List(Token("the", "DT", "the"),
    //         Token("United", "NNP", "united"), Token("States", "NNPS", "state"))),
    //       Token("of", "IN", "of")),
    //     Token("of", "IN", "of")),
    //   Token("of", "IN", "of")))

    { env =>
      val country = env.newNode()
          .out(NameLabel, "the United States")

      val president = env.newNode()
          .in(country, P.hasHeadOfState)

      val wife = env.newNode()
          .out(P.hasGender, Q.female)
          .in(president, P.hasSpouse)

      val daughter = env.newNode()
          .out(P.hasGender, Q.female)
          .in(wife, P.hasChild)

      List(daughter)
    },
    ("4",
      """
        |{ ?4  wdt:P21     wd:Q6581072 .
        |  ?3  wdt:P40     ?4 ;
        |      wdt:P21     wd:Q6581072 .
        |  ?2  wdt:P26     ?3 .
        |  ?1  wdt:P35     ?2 ;
        |      rdfs:label  "the United States"@en
        |}
      """.stripMargin,
      None))


  test("Who/WP/who is/VBZ/be the/DT/the son/NN/son of/IN/of the/DT/the actor/NN/actor of/IN/of"
    + " \"/``/\" I/PRP/i ,/,/, Robot/NNP/robot \"/''/\"",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"), Token("son", "NN", "son"))),
    //   RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"), Token("actor", "NN", "actor"))),
    //     NamedQuery(List(Token("I", "PRP", "i"), Token(",", ",", ","), Token("Robot", "NNP", "robot"))),
    //     Token("of", "IN", "of")),
    //   Token("of", "IN", "of")))

    { env =>
      // TODO: whitespace should be preserved (no space between 'I' and comma)
      val movie = env.newNode()
          .out(NameLabel, "I , Robot")

      val actor = env.newNode()
          .out(P.hasOccupation, Q.actor)
          .in(movie, P.hasCastMember)

      val son = env.newNode()
          .out(P.hasGender, Q.male)
          .in(actor, P.hasChild)

      List(son)
    },
    ("3",
      """
        |{ ?3  wdt:P21  wd:Q6581097
        |  { ?2  wdt:P40  ?3
        |    { ?2 p:P106/(v:P106/(wdt:P279)*) wd:Q33999
        |      { ?1  wdt:P161    ?2 ;
        |            rdfs:label  "I , Robot"@en
        |      }
        |    }
        |  }
        |}
      """.stripMargin,
      None))


  test("children/NNS/child of/IN/of all/DT/all presidents/NNS/president of/IN/of the/DT/the US/NNP/us",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("children", "NNS", "child"))),
    //   RelationshipQuery(NamedQuery(List(Token("all", "DT", "all"), Token("presidents", "NNS", "president"))),
    //     NamedQuery(List(Token("the", "DT", "the"), Token("US", "NNP", "us"))),
    //     Token("of", "IN", "of")),
    //   Token("of", "IN", "of")))

    { env =>
      val country = env.newNode()
          .out(NameLabel, "the US")

      val president = env.newNode()
          .in(country, P.hasHeadOfState)

      val child = env.newNode()
          .in(president, P.hasChild)

      List(child)
    },
    ("3",
      """
        |{ ?2  wdt:P40     ?3 .
        |  ?1  wdt:P35     ?2 ;
        |      rdfs:label  "the US"@en
        |}
      """.stripMargin,
      None))


  test("List/VB/list movies/NNS/movie directed/VBN/direct by/IN/by Quentin/NNP/quentin Tarantino/NNP/tarantino",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("movies", "NNS", "movie"))),
    //   PropertyWithFilter(List(Token("directed", "VBN", "direct")),
    //     FilterWithModifier(List(Token("by", "IN", "by")),
    //       NamedValue(List(Token("Quentin", "NNP", "quentin"), Token("Tarantino", "NNP", "tarantino")))))))

    { env =>
      val movie = env.newNode()
          .out(P.isA, Q.movie)

      val director = env.newNode()
          .out(NameLabel, "Quentin Tarantino")

      List(movie.out(P.hasDirector, director))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q11424
        |  { ?1  wdt:P57     ?2 .
        |    ?2  rdfs:label  "Quentin Tarantino"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("Which/WDT/which movies/NN/movie did/VBD/do Mel/NNP/mel Gibson/NNP/gibson direct/VB/direct",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("movies", "NN", "movie"))),
    //   InversePropertyWithFilter(List(Token("did", "VBD", "do"), Token("direct", "VB", "direct")),
    //     PlainFilter(NamedValue(List(Token("Mel", "NNP", "mel"), Token("Gibson", "NNP", "gibson")))))))

    { env =>
      val movie = env.newNode()
          .out(P.isA, Q.movie)

      val director = env.newNode()
          .out(NameLabel, "Mel Gibson")

      List(movie.out(P.hasDirector, director))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q11424
        |  { ?1  wdt:P57     ?2 .
        |    ?2  rdfs:label  "Mel Gibson"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("actors/NNP/actor",

    // ListQuestion(NamedQuery(List(Token("actors", "NNP", "actor"))))

    { env =>
      val actor = env.newNode()
          .out(P.isA, Q.actor)
          .or(out(P.hasOccupation, Q.actor))

      List(actor)
    },
    ("1",
      """
        |{   { ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q33999 }
        |  UNION
        |    { ?1 p:P106/(v:P106/(wdt:P279)*) wd:Q33999 }
        |}
      """.stripMargin,
      None))


  test("what/WDT/what languages/NNS/language are/VBP/be spoken/VBN/speak in/IN/in Switzerland/NNP/switzerland",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("languages", "NNS", "language"))),
    //   PropertyWithFilter(List(Token("are", "VBP", "be"), Token("spoken", "VBN", "speak")),
    //     FilterWithModifier(List(Token("in", "IN", "in")),
    //       NamedValue(List(Token("Switzerland", "NNP", "switzerland")))))))

    { env =>
      val language = env.newNode()
          .out(P.isA, Q.language)

      val country = env.newNode()
          .out(NameLabel, "Switzerland")

      List(language
          .in(country, P.hasOfficialLanguage))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q34770
        |  { ?2  wdt:P37     ?1 ;
        |        rdfs:label  "Switzerland"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("Which/WDT/which books/NN/book did/VBD/do Orwell/NNP/orwell or/CC/or Shakespeare/NNP/shakespeare write/VB/write",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NN", "book"))),
    //   InversePropertyWithFilter(List(Token("did", "VBD", "do"), Token("write", "VB", "write")),
    //     PlainFilter(OrValue(List(NamedValue(List(Token("Orwell", "NNP", "orwell"))),
    //       NamedValue(List(Token("Shakespeare", "NNP", "shakespeare")))))))))

    { env =>
      val book = env.newNode()
          .out(P.isA, Q.book)

      val orwell = env.newNode()
          .out(NameLabel, "Orwell")

      val shakespeare = env.newNode()
          .out(NameLabel, "Shakespeare")

      List(book
          .and(out(P.hasAuthor, orwell)
               or out(P.hasAuthor, shakespeare)))
    },
    ("1",
      """
        |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q571
        |    { ?1  wdt:P50     ?2 .
        |      ?2  rdfs:label  "Orwell"@en
        |    }
        |  UNION
        |    { ?1  wdt:P50     ?3 .
        |      ?3  rdfs:label  "Shakespeare"@en
        |    }
        |}
      """.stripMargin,
      None))


  test("which/WDT/which books/NN/book were/VBD/be authored/VBN/author by/IN/by George/NNP/george Orwell/NNP/orwell",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NN", "book"))),
    //   PropertyWithFilter(List(Token("were", "VBD", "be"), Token("authored", "VBN", "author")),
    //     FilterWithModifier(List(Token("by", "IN", "by")),
    //       NamedValue(List(Token("George", "NNP", "george"), Token("Orwell", "NNP", "orwell")))))))

    { env =>
      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      List(book
          .out(P.hasAuthor, author))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q571
        |  { ?1  wdt:P50     ?2 .
        |    ?2  rdfs:label  "George Orwell"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("which/WDT/which instrument/NN/instrument did/VBD/do John/NNP/john Lennon/NNP/lennon play/VB/play",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("instrument", "NN", "instrument"))),
    //   InversePropertyWithFilter(List(Token("did", "VBD", "do"), Token("play", "VB", "play")),
    //     PlainFilter(NamedValue(List(Token("John", "NNP", "john"), Token("Lennon", "NNP", "lennon")))))))

    { env =>
      val instrument = env.newNode()
          .out(P.isA, Q.musicalInstrument)

      val musician = env.newNode()
          .out(NameLabel, "John Lennon")

      List(instrument
          .in(musician, P.playsInstrument))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q34379
        |  { ?2  wdt:P1303   ?1 ;
        |        rdfs:label  "John Lennon"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("Who/WP/who wrote/VBD/write \"/``/\" Le/NNP/le Petit/NNP/petit Prince/NNP/prince \"/''/\""
    + " and/CC/and \"/``/\" Vol/NNP/vol de/NNP/de Nuit/NNP/nuit \"/''/\"",

    // PersonListQuestion(PropertyWithFilter(List(Token("wrote", "VBD", "write")),
    //   PlainFilter(AndValue(List(NamedValue(List(Token("Le", "NNP", "le"),
    //     Token("Petit", "NNP", "petit"), Token("Prince", "NNP", "prince"))),
    //     NamedValue(List(Token("Vol", "NNP", "vol"), Token("de", "NNP", "de"), Token("Nuit", "NNP", "nuit"))))))))

    { env =>
      val person = env.newNode()
          .out(P.isA, Q.human)

      val book = env.newNode()
          .out(NameLabel, "Le Petit Prince")

      val book2 = env.newNode()
          .out(NameLabel, "Vol de Nuit")

      List(person
          .in(book, P.hasAuthor)
          .in(book2, P.hasAuthor))
    },
    ("1",
      """
        |{ { ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
        |    { ?2  wdt:P50     ?1 ;
        |          rdfs:label  "Le Petit Prince"@en
        |    }
        |  }
        |  ?3  wdt:P50     ?1 ;
        |      rdfs:label  "Vol de Nuit"@en
        |}
      """.stripMargin,
      None))


  test("What/WP/what did/VBD/do George/NNP/george Orwell/NNP/orwell write/VB/write",

    // ThingListQuestion(InversePropertyWithFilter(List(Token("did", "VBD", "do"),
    //   Token("write", "VB", "write")),
    //   PlainFilter(NamedValue(List(Token("George", "NNP", "george"), Token("Orwell", "NNP", "orwell"))))))

    { env =>
      val thing = env.newNode()

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      List(thing
          .out(P.hasAuthor, author))
    },
    ("1",
      """
        |{ ?1  wdt:P50     ?2 .
        |  ?2  rdfs:label  "George Orwell"@en
        |}
      """.stripMargin,
      None))


  test("What/WP/what was/VBD/be authored/VBN/author by/IN/by George/NNP/george Orwell/NNP/orwell",

    // ThingListQuestion(PropertyWithFilter(List(Token("was", "VBD", "be"), Token("authored", "VBN", "author")),
    //   FilterWithModifier(List(Token("by", "IN", "by")),
    //     NamedValue(List(Token("George", "NNP", "george"), Token("Orwell", "NNP", "orwell"))))))

    { env =>
      val thing = env.newNode()

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      List(thing
          .out(P.hasAuthor, author))
    },
    ("1",
      """
        |{ ?1  wdt:P50     ?2 .
        |  ?2  rdfs:label  "George Orwell"@en
        |}
      """.stripMargin,
      None))


  test("What/WP/what books/NNP/book were/VBD/be authored/VBN/author by/IN/by George/NNP/george Orwell/NNP/orwell",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NNP", "book"))),
    //   PropertyWithFilter(List(Token("were", "VBD", "be"), Token("authored", "VBN", "author")),
    //     FilterWithModifier(List(Token("by", "IN", "by")),
    //       NamedValue(List(Token("George", "NNP", "george"), Token("Orwell", "NNP", "orwell")))))))

    { env =>
      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      List(book
          .out(P.hasAuthor, author))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q571
        |  { ?1  wdt:P50     ?2 .
        |    ?2  rdfs:label  "George Orwell"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("authors/NNS/author who/WP/who died/VBD/die",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("authors", "NNS", "author"))),
    //   NamedProperty(List(Token("died", "VBD", "die")))))

    { env =>
      val author = env.newNode()
          .in(env.newNode(), P.hasAuthor)

      val place = env.newNode()

      List(author
          .and(out(P.hasDateOfDeath, place)
               or out(P.hasPlaceOfDeath, place)))
    },
    ("1",
      """
        |{ ?2  wdt:P50  ?1
        |    { ?1  wdt:P570  ?3 }
        |  UNION
        |    { ?1  wdt:P20/(wdt:P131)*  ?3 }
        |}
      """.stripMargin,
      None))


  test("What/WDT/what actor/NN/actor married/VBD/marry"
    + " John/NNP/john F./NNP/f. Kennedy/NNP/kennedy 's/POS/'s sister/NN/sister",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("actor", "NN", "actor"))),
    //   PropertyWithFilter(List(Token("married", "VBD", "marry")),
    //     PlainFilter(ValueRelationship(NamedValue(List(Token("sister", "NN", "sister"))),
    //       NamedValue(List(Token("John", "NNP", "john"), Token("F.", "NNP", "f."),
    //         Token("Kennedy", "NNP", "kennedy"))))))))

    { env =>
      val actor = env.newNode()
          .out(P.isA, Q.actor)
          .or(out(P.hasOccupation, Q.actor))

      val kennedy = env.newNode()
          .out(NameLabel, "John F. Kennedy")

      val sister = env.newNode()
          .in(kennedy, P.hasSister)

      List(actor.out(P.hasSpouse, sister))
    },
    ("1",
      """
        |{   { ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q33999 }
        |  UNION
        |    { ?1 p:P106/(v:P106/(wdt:P279)*) wd:Q33999 }
        |  ?1  wdt:P26     ?3 .
        |  ?2  wdt:P9      ?3 ;
        |      rdfs:label  "John F. Kennedy"@en
        |}
      """.stripMargin,
      None))


  test("Who/WP/who did/VBD/do Bill/NNP/bill Clinton/NNP/clinton 's/POS/'s daughter/NN/daughter marry/VB/marry",

    // PersonListQuestion(InversePropertyWithFilter(List(Token("did", "VBD", "do"), Token("marry", "VB", "marry")),
    //   PlainFilter(ValueRelationship(NamedValue(List(Token("daughter", "NN", "daughter"))),
    //     NamedValue(List(Token("Bill", "NNP", "bill"), Token("Clinton", "NNP", "clinton")))))))

    { env =>
      val person = env.newNode()
          .out(P.isA, Q.human)

      val bill = env.newNode()
          .out(NameLabel, "Bill Clinton")

      val daughter = env.newNode()
          .out(P.hasGender, Q.female)
          .in(bill, P.hasChild)

      List(person.out(P.hasSpouse, daughter))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
        |  { ?1  wdt:P26     ?3 .
        |    ?3  wdt:P21     wd:Q6581072 .
        |    ?2  wdt:P40     ?3 ;
        |        rdfs:label  "Bill Clinton"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("Clinton/NNP/clinton 's/POS/'s children/NNS/child and/CC/and grandchildren/NNS/grandchild",

    // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("children", "NNS", "child"))),
    //   NamedQuery(List(Token("grandchildren", "NNS", "grandchild"))))),
    //   NamedQuery(List(Token("Clinton", "NNP", "clinton"))),
    //   Token("'s", "POS", "'s")))

    { env =>
      val bill = env.newNode()
          .out(NameLabel, "Clinton")

      val child = env.newNode()
          .in(bill, P.hasChild)
      // NOTE: another child node, but might be child
      val child2 = env.newNode()
          .in(bill, P.hasChild)

      val grandchild = env.newNode()
          .in(child2, P.hasChild)

      List(child, grandchild)
    },
    ("2",
      """
        |{ ?1  wdt:P40     ?2 ;
        |      rdfs:label  "Clinton"@en
        |}
      """.stripMargin,
      None),
    ("4",
      """
        |{ ?3  wdt:P40     ?4 .
        |  ?1  wdt:P40     ?3 ;
        |      rdfs:label  "Clinton"@en
        |}
      """.stripMargin,
      None))


  test("What/WP/what are/VBP/be the/DT/the population/NN/population"
    + " of/IN/of China/NNP/china and/CC/and the/DT/the USA/NNP/usa",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
    //   Token("population", "NN", "population"))),
    //   AndQuery(List(NamedQuery(List(Token("China", "NNP", "china"))),
    //     NamedQuery(List(Token("the", "DT", "the"), Token("USA", "NNP", "usa"))))),
    //   Token("of", "IN", "of")))

    { env =>
      val china = env.newNode()
          .out(NameLabel, "China")

      val usa = env.newNode()
          .out(NameLabel, "the USA")

      val populationOfChina = env.newNode().in(china, P.hasPopulation)

      val populationOfUSA = env.newNode().in(usa, P.hasPopulation)

      List(populationOfChina, populationOfUSA)
    },
    ("3",
      """
        |{ ?1  wdt:P1082   ?3 ;
        |      rdfs:label  "China"@en
        |}
      """.stripMargin,
      None),
    ("4",
      """
        |{ ?2  wdt:P1082   ?4 ;
        |      rdfs:label  "the USA"@en
        |}
      """.stripMargin,
      None))


  test("the/DT/the population/NNP/population of/IN/of Japan/NNP/japan and/CC/and China/NNP/china",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
    //   Token("population", "NNP", "population"))),
    //   AndQuery(List(NamedQuery(List(Token("Japan", "NNP", "japan"))),
    //     NamedQuery(List(Token("China", "NNP", "china"))))),
    //   Token("of", "IN", "of")))

    { env =>
      val japan = env.newNode()
          .out(NameLabel, "Japan")

      val china = env.newNode()
          .out(NameLabel, "China")

      val populationOfJapan = env.newNode().in(japan, P.hasPopulation)

      val populationOfChina = env.newNode().in(china, P.hasPopulation)

      List(populationOfJapan, populationOfChina)
    },
    ("3",
      """
        |{ ?1  wdt:P1082   ?3 ;
        |      rdfs:label  "Japan"@en
        |}
      """.stripMargin,
      None),
    ("4",
      """
        |{ ?2  wdt:P1082   ?4 ;
        |      rdfs:label  "China"@en
        |}
      """.stripMargin,
      None))


  test("the/DT/the population/NNP/population and/CC/and area/NNP/area"
    + " of/IN/of Japan/NNP/japan and/CC/and China/NNP/china",

    // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("the", "DT", "the"),
    //     Token("population", "NNP", "population"))),
    //   NamedQuery(List(Token("area", "NNP", "area"))))),
    //   AndQuery(List(NamedQuery(List(Token("Japan", "NNP", "japan"))),
    //     NamedQuery(List(Token("China", "NNP", "china"))))),
    //   Token("of", "IN", "of")))

    { env =>
      val japan = env.newNode()
          .out(NameLabel, "Japan")

      val china = env.newNode()
          .out(NameLabel, "China")

      val populationOfJapan = env.newNode()
          .in(japan, P.hasPopulation)

      val populationOfChina = env.newNode()
          .in(china, P.hasPopulation)

      val areaOfJapan = env.newNode()
          .in(japan, P.hasArea)

      val areaOfChina = env.newNode()
          .in(china, P.hasArea)

      List(populationOfJapan, populationOfChina,
        areaOfJapan, areaOfChina)
    },
    ("3",
      """
        |{ ?1  wdt:P1082   ?3 ;
        |      rdfs:label  "Japan"@en
        |}
      """.stripMargin,
      None),
    ("4",
      """
        |{ ?2  wdt:P1082   ?4 ;
        |      rdfs:label  "China"@en
        |}
      """.stripMargin,
      None),
    ("5",
      """
        |{ ?1  wdt:P2046   ?5 ;
        |      rdfs:label  "Japan"@en
        |}
      """.stripMargin,
      None),
    ("6",
      """
        |{ ?2  wdt:P2046   ?6 ;
        |      rdfs:label  "China"@en
        |}
      """.stripMargin,
      None))


  test("the/DT/the population/NN/population ,/,/, land/NN/land area/NN/area and/CC/and capitals/NNP/capital of/IN/of"
    + " Japan/NNP/japan ,/,/, India/NNP/india and/CC/and China/NNP/china",

    // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("the", "DT", "the"),
    //     Token("population", "NN", "population"))),
    //   NamedQuery(List(Token("land", "NN", "land"), Token("area", "NN", "area"))),
    //   NamedQuery(List(Token("capitals", "NNP", "capital"))))),
    //   AndQuery(List(NamedQuery(List(Token("Japan", "NNP", "japan"))),
    //     NamedQuery(List(Token("India", "NNP", "india"))),
    //     NamedQuery(List(Token("China", "NNP", "china"))))),
    //   Token("of", "IN", "of")))

    { env =>
      val japan = env.newNode()
          .out(NameLabel, "Japan")

      val india = env.newNode()
          .out(NameLabel, "India")

      val china = env.newNode()
          .out(NameLabel, "China")

      val populationOfJapan = env.newNode()
          .in(japan, P.hasPopulation)

      val populationOfIndia = env.newNode()
          .in(india, P.hasPopulation)

      val populationOfChina = env.newNode()
          .in(china, P.hasPopulation)

      val areaOfJapan = env.newNode()
          .in(japan, P.hasArea)

      val areaOfIndia = env.newNode()
          .in(india, P.hasArea)

      val areaOfChina = env.newNode()
          .in(china, P.hasArea)

      val capitalOfJapan = env.newNode()
          .in(japan, P.hasCapital)

      val capitalOfIndia = env.newNode()
          .in(india, P.hasCapital)

      val capitalOfChina = env.newNode()
          .in(china, P.hasCapital)

      List(populationOfJapan, populationOfIndia, populationOfChina,
        areaOfJapan, areaOfIndia, areaOfChina,
        capitalOfJapan, capitalOfIndia, capitalOfChina)
    },
    ("4",
      """
        |{ ?1  wdt:P1082   ?4 ;
        |      rdfs:label  "Japan"@en
        |}
      """.stripMargin,
      None),
    ("5",
      """
        |{ ?2  wdt:P1082   ?5 ;
        |      rdfs:label  "India"@en
        |}
      """.stripMargin,
      None),
    ("6",
      """
        |{ ?3  wdt:P1082   ?6 ;
        |      rdfs:label  "China"@en
        |}
      """.stripMargin,
      None),
    ("7",
      """
        |{ ?1  wdt:P2046   ?7 ;
        |      rdfs:label  "Japan"@en
        |}
      """.stripMargin,
      None),
    ("8",
      """
        |{ ?2  wdt:P2046   ?8 ;
        |      rdfs:label  "India"@en
        |}
      """.stripMargin,
      None),
    ("9",
      """
        |{ ?3  wdt:P2046   ?9 ;
        |      rdfs:label  "China"@en
        |}
      """.stripMargin,
      None),
    ("10",
      """
        |{ ?1  wdt:P36     ?10 ;
        |      rdfs:label  "Japan"@en
        |}
      """.stripMargin,
      None),
    ("11",
      """
        |{ ?2  wdt:P36     ?11 ;
        |      rdfs:label  "India"@en
        |}
      """.stripMargin,
      None),
    ("12",
      """
        |{ ?3  wdt:P36     ?12 ;
        |      rdfs:label  "China"@en
        |}
      """.stripMargin,
      None))


  test("Japan/NNP/japan and/CC/and China/NNP/china 's/POS/'s population/NNP/population and/CC/and area/NNP/area",

    // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("population", "NNP", "population"))),
    //   NamedQuery(List(Token("area", "NNP", "area"))))),
    //   AndQuery(List(NamedQuery(List(Token("Japan", "NNP", "japan"))),
    //     NamedQuery(List(Token("China", "NNP", "china"))))),
    //   Token("'s", "POS", "'s")))

    { env =>
      val japan = env.newNode()
          .out(NameLabel, "Japan")

      val china = env.newNode()
          .out(NameLabel, "China")

      val populationOfJapan = env.newNode()
          .in(japan, P.hasPopulation)

      val populationOfChina = env.newNode()
          .in(china, P.hasPopulation)

      val areaOfJapan = env.newNode()
          .in(japan, P.hasArea)

      val areaOfChina = env.newNode()
          .in(china, P.hasArea)

      List(populationOfJapan, populationOfChina,
        areaOfJapan, areaOfChina)
    },
    ("3",
      """
        |{ ?1  wdt:P1082   ?3 ;
        |      rdfs:label  "Japan"@en
        |}
      """.stripMargin,
      None),
    ("4",
      """
        |{ ?2  wdt:P1082   ?4 ;
        |      rdfs:label  "China"@en
        |}
      """.stripMargin,
      None),
    ("5",
      """
        |{ ?1  wdt:P2046   ?5 ;
        |      rdfs:label  "Japan"@en
        |}
      """.stripMargin,
      None),
    ("6",
      """
        |{ ?2  wdt:P2046   ?6 ;
        |      rdfs:label  "China"@en
        |}
      """.stripMargin,
      None))


  test("children/NNS/child and/CC/and grandchildren/NNS/grandchild of/IN/of Clinton/NNP/clinton",

    // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("children", "NNS", "child"))),
    //   NamedQuery(List(Token("grand", "JJ", "grand"), Token("children", "NNS", "child"))))),
    //   NamedQuery(List(Token("Clinton", "NNP", "clinton"))),
    //   Token("of", "IN", "of")))

    { env =>
      val bill = env.newNode()
          .out(NameLabel, "Clinton")

      val child = env.newNode()
          .in(bill, P.hasChild)
      // NOTE: another child node, but might be child
      val child2 = env.newNode()
          .in(bill, P.hasChild)

      val grandchild = env.newNode()
          .in(child2, P.hasChild)

      List(child, grandchild)
    },
    ("2",
      """
        |{ ?1  wdt:P40     ?2 ;
        |      rdfs:label  "Clinton"@en
        |}
      """.stripMargin,
      None),
    ("4",
      """
        |{ ?3  wdt:P40     ?4 .
        |  ?1  wdt:P40     ?3 ;
        |      rdfs:label  "Clinton"@en
        |}
      """.stripMargin,
      None))


  test("who/WP/who wrote/VBD/write books/NNS/book",

    // PersonListQuestion(PropertyWithFilter(List(Token("wrote", "VBD", "write")),
    //     PlainFilter(NamedValue(List(Token("books", "NNS", "book"))))))

    { env =>
      val person = env.newNode()
          .out(P.isA, Q.human)

      val book = env.newNode()
          .out(P.isA, Q.book)

      List(person.in(book, P.hasAuthor))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
        |  { ?2  wdt:P50  ?1
        |    { ?2 p:P31/(v:P31/(wdt:P279)*) wd:Q571 }
        |  }
        |}
      """.stripMargin,
      None))


  test("presidents/NNS/president that/WDT/that have/VBP/have children/NNS/child",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("presidents", "NNS", "president"))),
    //   PropertyWithFilter(List(Token("have", "VBP", "have")),
    //     PlainFilter(NamedValue(List(Token("children", "NNS", "child")))))))

    { env =>
      val president = env.newNode()
          .out(P.holdsPosition, Q.president)

      val child = env.newNode()
          .in(env.newNode(), P.hasChild)

      List(president.out(P.hasChild, child))
    },
    ("1",
      """
        |{ ?1 p:P39/(v:P39/(wdt:P279)*) wd:Q30461
        |  { ?1  wdt:P40  ?2 .
        |    ?3  wdt:P40  ?2
        |  }
        |}
      """.stripMargin,
      None))


  test("what/WP/what are/VBP/be the/DT/the largest/JJS/large cities/NNS/city in/IN/in europe/NN/europe",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("the", "DT", "the"),
    //     Token("largest", "JJS", "large"), Token("cities", "NNS", "city"))),
    //   PropertyWithFilter(List(),
    //     FilterWithModifier(List(Token("in", "IN", "in")),
    //       NamedValue(List(Token("europe", "NN", "europe")))))))

    { env =>
      val city = env.newNode()
          .out(P.isA, Q.city)

      val population = env.newNode()
          .order(Descending)

      val europe = env.newNode()
          .out(NameLabel, "europe")

      List(city
          .out(P.hasPopulation, population)
          .out(P.isLocatedIn, europe))
    },
    ("1",
      """
        |{ { ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q515
        |    { ?1  wdt:P1082  ?2 }
        |  }
        |  { ?1 (wdt:P131)+ ?3
        |    { ?3  rdfs:label  "europe"@en }
        |  }
        |}
      """.stripMargin,
      Some("DESC(?2)")))


  test("List/VB/list books/NNS/book by/IN/by George/NNP/george Orwell/NNP/orwell",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NNS", "book"))),
    //   PropertyWithFilter(List(),
    //     FilterWithModifier(List(Token("by", "IN", "by")),
    //       NamedValue(List(Token("George", "NNP", "george"), Token("Orwell", "NNP", "orwell")))))))

    { env =>
      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      List(book.out(P.hasAuthor, author))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q571
        |  { ?1  wdt:P50     ?2 .
        |    ?2  rdfs:label  "George Orwell"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("Which/WDT/which city/NN/city is/VBZ/be bigger/JJR/big than/IN/than New/NNP/new York/NNP/york City/NNP/city",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("city", "NN", "city"))),
    //   PropertyWithFilter(List(Token("is", "VBZ", "be")),
    //     FilterWithComparativeModifier(List(Token("bigger", "JJR", "big"), Token("than", "IN", "than")),
    //       NamedValue(List(Token("New", "NNP", "new"), Token("York", "NNP", "york"),
    //         Token("City", "NNP", "city")))))))

    { env =>
      val city = env.newNode()
          .out(P.isA, Q.city)

      val newYorkCity = env.newNode()
          .out(NameLabel, "New York City")

      val otherArea = env.newNode()
          .in(newYorkCity, P.hasArea)

      val area = env.newNode()
          .filter(GreaterThanFilter(otherArea))

      List(city.out(P.hasArea, area))
    },
    ("1",
      """
        |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q515
        |  { ?1  wdt:P2046   ?4 .
        |    ?2  wdt:P2046   ?3 ;
        |        rdfs:label  "New York City"@en
        |    FILTER ( ?4 > ?3 )
        |  }
        |}
      """.stripMargin,
      None))


  test("who/WP/who is/VBZ/be older/JJR/old than/IN/than Obama/NNP/obama",

    // PersonListQuestion(PropertyWithFilter(List(Token("is", "VBZ", "be")),
    //   FilterWithComparativeModifier(List(Token("older", "JJR", "old"), Token("than", "IN", "than")),
    //     NamedValue(List(Token("Obama", "NNP", "obama"))))))

    { env =>
      val person = env.newNode()
          .out(P.isA, Q.human)

      val obama = env.newNode()
          .out(NameLabel, "Obama")

      val otherBirthDate = env.newNode()
          .in(obama, P.hasDateOfBirth)

      val birthDate = env.newNode()
          .filter(LessThanFilter(otherBirthDate))

      List(person.out(P.hasDateOfBirth, birthDate))
    },
    ("1",
      """
        |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q5
        |  { ?1  wdt:P569    ?4 .
        |    ?2  wdt:P569    ?3 ;
        |        rdfs:label  "Obama"@en
        |    FILTER ( ?4 < ?3 )
        |  }
        |}
      """.stripMargin,
      None))


  test("which/WDT/which mountains/NNS/mountain are/VBP/be "
      + "more/JJR/more than/IN/than 1000/CD/1000 meters/NNS/meter high/JJ/high",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("mountains", "NNS", "mountain"))),
    //   AdjectivePropertyWithFilter(List(Token("are", "VBP", "be"), Token("high", "JJ", "high")),
    //     FilterWithComparativeModifier(List(Token("more", "JJR", "more"), Token("than", "IN", "than")),
    //       NumberWithUnit(List(Token("1000", "CD", "1000")), List(Token("meters", "NNS", "meter")))))))

    { env =>
      val mountain = env.newNode()
          .out(P.isA, Q.mountain)

      val minElevation: WikidataNode = (1000.0, U.meter)

      val elevation = env.newNode()
          .filter(GreaterThanFilter(minElevation))

      List(mountain.out(P.hasElevation, elevation))
    },
    ("1",
      """
        |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q8502
        |  { ?1  wdt:P2044  ?2
        |    FILTER ( ?2 > "1000.0"^^xsd:integer )
        |  }
        |}
      """.stripMargin,
      None))


  test("Which/WDT/which cities/NNS/city have/VBP/have more/JJR/more than/IN/than "
      + "two/CD/two million/CD/million inhabitants/NNS/inhabitant",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("cities", "NNS", "city"))),
    //   PropertyWithFilter(List(Token("have", "VBP", "have")),
    //     FilterWithComparativeModifier(List(Token("more", "JJR", "more"), Token("than", "IN", "than")),
    //       NumberWithUnit(List(Token("two", "CD", "two"), Token("million", "CD", "million")),
    //         List(Token("inhabitants", "NNS", "inhabitant")))))))

    { env =>
      val city = env.newNode()
          .out(P.isA, Q.city)

      val minPopulation: WikidataNode = 2000000.0

      val population = env.newNode()
          .filter(GreaterThanFilter(minPopulation))

      List(city.out(P.hasPopulation, population))
    },
    ("1",
      """
        |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q515
        |  { ?1  wdt:P1082  ?2
        |    FILTER ( ?2 > "2000000.0"^^xsd:double )
        |  }
        |}
      """.stripMargin,
      None))


  // TODO: adjective "californian": P.isLocatedIn

  test("In/IN/in which/WDT/which californian/JJ/californian cities/NNS/city "
    + "live/VBP/live more/JJR/more than/IN/than 2/CD/2 million/CD/million people/NNS/people",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("californian", "JJ", "californian"),
    //   Token("cities", "NNS", "city"))),
    //   PropertyWithFilter(List(Token("live", "VBP", "live")),
    //     FilterWithComparativeModifier(List(Token("more", "JJR", "more"), Token("than", "IN", "than")),
    //       NumberWithUnit(List(Token("2", "CD", "2"), Token("million", "CD", "million")),
    //         List(Token("people", "NNS", "people")))))))

    { env =>
      val city = env.newNode()
          .out(P.isA, Q.city)

      val minPopulation: WikidataNode = 2000000.0

      val population = env.newNode()
          .filter(GreaterThanFilter(minPopulation))

      List(city.out(P.hasPopulation, population))
    },
    ("1",
      """
        |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q515
        |  { ?1  wdt:P1082  ?2
        |    FILTER ( ?2 > "2000000.0"^^xsd:double )
        |  }
        |}
      """.stripMargin,
      None))


  test(s"who/WP/who is/VBD/be the/DT/the discoverer/NNS/discoverer of/IN/of Pluto/NNP/pluto",

    { env =>
      val pluto = env.newNode()
          .out(NameLabel, "Pluto")

      val discoverer = env.newNode()
          .in(pluto, P.hasDiscovererOrInventor)

      List(discoverer)
    },
    ("2",
      """
        |{ ?1  wdt:P61     ?2 ;
        |      rdfs:label  "Pluto"@en
        |}
      """.stripMargin,
      None))


  // TODO: implement aggregates in SPARQL compiler

  test("who/WP/who discovered/VBD/discover the/DT/the most/JJS/most planets/NNS/planet",

    // PersonListQuestion(PropertyWithFilter(List(Token("discovered", "VBD", "discover")),
    //   PlainFilter(NamedValue(List(Token("the", "DT", "the"), Token("most", "JJS", "most"),
    //     Token("planets", "NNS", "planet"))))))

    { env =>
      val discoverer = env.newNode()
          .out(P.isA, Q.human)

      val planet = env.newNode()
          .out(P.isA, Q.planet)
          .aggregate(Count)
          .order(Descending)

      List(discoverer
          .in(planet, P.hasDiscovererOrInventor))
    })


  test("who/WP/who are/VBP/be the/DT/the children/NNS/child of/IN/of Clinton/NNP/clinton 's/POS/'s spouse/NNS/spouse",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"), Token("children", "NNS", "child"))),
    //   RelationshipQuery(NamedQuery(List(Token("spouse", "NNS", "spouse"))),
    //     NamedQuery(List(Token("Clinton", "NNP", "clinton"))), Token("'s", "POS", "'s")), Token("of", "IN", "of")))

    { env =>
      val clinton = env.newNode()
          .out(NameLabel, "Clinton")

      val spouse = env.newNode()
          .in(clinton, P.hasSpouse)

      val child = env.newNode()
          .in(spouse, P.hasChild)

      List(child)
    },
    ("3",
      """
        |{ ?2  wdt:P40     ?3 .
        |  ?1  wdt:P26     ?2 ;
        |      rdfs:label  "Clinton"@en
        |}
      """.stripMargin,
      None))


  test("Which/WDT/which books/NNS/book were/VBD/be written/VBN/write by/IN/by Jane/NNP/jane Austen/NNP/austen",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NNS", "book"))),
    //   PropertyWithFilter(List(Token("were", "VBD", "be"), Token("written", "VBN", "write")),
    //     FilterWithModifier(List(Token("by", "IN", "by")),
    //       NamedValue(List(Token("Jane", "NNP", "jane"), Token("Austen", "NNP", "austen")))))))

    { env =>
      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "Jane Austen")

      List(book.out(P.hasAuthor, author))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q571
        |  { ?1  wdt:P50     ?2 .
        |    ?2  rdfs:label  "Jane Austen"@en
        |  }
        |}
      """.stripMargin,
      None))


  test("Which/WDT/which country/NN/country was/VBD/be Obama/NNP/obama born/VBN/bear in/IN/in",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("country", "NN", "country"))),
    //   InversePropertyWithFilter(List(Token("was", "VBD", "be"), Token("born", "VBN", "bear"),
    //     Token("in", "IN", "in")),
    //     PlainFilter(NamedValue(List(Token("Obama", "NNP", "obama")))))))

    { env =>
      val country = env.newNode()
        .out(P.isA, Q.country)

      val obama = env.newNode()
        .out(NameLabel, "Obama")

      val place = env.newNode()
        .in(obama, P.hasPlaceOfBirth)

      List(country
        .in(place, P.country))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q6256
        |  { ?3  wdt:P17  ?1
        |    { ?2 wdt:P19/(wdt:P131)* ?3
        |      { ?2  rdfs:label  "Obama"@en }
        |    }
        |  }
        |}
      """.stripMargin,
      None))


  // TODO: implement YearLabel as binding of year of root variable

  test("Which/WDT/which year/NN/year was/VBD/be Obama/NNP/obama born/VBN/bear in/IN/in",

    // ListQuestion(QueryWithProperty(NamedQuery(List(Token("year", "NN", "year"))),
    //   InversePropertyWithFilter(List(Token("was", "VBD", "be"), Token("born", "VBN", "bear"),
    //     Token("in", "IN", "in")),
    //     PlainFilter(NamedValue(List(Token("Obama", "NNP", "obama")))))))

    { env =>
      val year = env.newNode()
          .out(P.isA, Q.year)

      val obama = env.newNode()
          .out(NameLabel, "Obama")

      val date = env.newNode()
          .in(obama, P.hasDateOfBirth)

      List(year
          .in(date, YearLabel))
    })


  test(s"What/WP/what are/VBP/be some/DT/some of/IN/of Seth/NNP/seth Gabel/NNP/gabel 's/POS/'s"
    + " father-in-law/NN/father-in-law 's/POS/'s movies/NNS/movie",

    // ListQuestion(RelationshipQuery(NamedQuery(List(Token("movies", "NNS", "movie"))),
    //   RelationshipQuery(NamedQuery(List(Token("father-in-law", "NN", "father-in-law"))),
    //     NamedQuery(List(Token("Seth", "NNP", "seth"), Token("Gabel", "NNP", "gabel"))),
    //     Token("'s", "POS", "'s")),
    //   Token("'s", "POS", "'s")))

    { env =>
      val sethGabel = env.newNode()
          .out(NameLabel, "Seth Gabel")

      val child = env.newNode()
          .out(P.hasSpouse, sethGabel)

      val fatherInLaw = env.newNode()
          .out(P.hasGender, Q.male)
          .out(P.hasChild, child)

      val movie = env.newNode()
          .out(P.isA, Q.movie)

      List(movie
          .out(P.hasDirector, fatherInLaw))
    },
    ("4",
      """
        |{ ?4 p:P31/(v:P31/(wdt:P279)*) wd:Q11424
        |  { ?4  wdt:P57     ?3 .
        |    ?3  wdt:P21     wd:Q6581097 ;
        |        wdt:P40     ?2 .
        |    ?2  wdt:P26     ?1 .
        |    ?1  rdfs:label  "Seth Gabel"@en
        |  }
        |}
      """.stripMargin,
      None))

  test("who/WP/who died/VBD/die in/IN/in Paris/NNP/paris",

    { env =>
      val person = env.newNode()
          .out(P.isA, Q.human)

      val paris = env.newNode()
          .out(NameLabel, "Paris")

      List(person
          .out(P.hasPlaceOfDeath, paris))
    },
    ("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
        |  { ?1 wdt:P20/(wdt:P131)* ?2
        |    { ?2  rdfs:label  "Paris"@en }
        |  }
        |}
      """.stripMargin,
      None))


  test("who/WP/who died/VBD/die before/IN/before 1900/CD/1900",

    { env =>
      val person = env.newNode()
          .out(P.isA, Q.human)

      val year: WikidataNode = Year.of(1900)

      val date = env.newNode()
          .filter(LessThanFilter(year))

      List(person
          .out(P.hasDateOfDeath, date))
    },
    ("1", """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
        |  { ?1  wdt:P570  ?2
        |    FILTER ( year(?2) < 1900 )
        |  }
        |}
      """.stripMargin,
      None))


  test("who/WP/who died/VBD/die in/IN/in 1900/CD/1900",
    { env =>
      val person = env.newNode()
        .out(P.isA, Q.human)

      val year: WikidataNode = Year.of(1900)

      List(person
        .out(P.hasDateOfDeath, year))
    },
    ("1", """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
        |  { ?1  wdt:P570  ?2
        |    FILTER ( year(?2) = 1900 )
        |  }
        |}
      """.stripMargin,
      None))

}