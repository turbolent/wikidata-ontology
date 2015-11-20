package com.turbolent.wikidataOntology

import java.nio.file.Paths
import java.time.Year

import com.turbolent.lemmatizer.Lemmatizer
import com.turbolent.questionCompiler.QuestionCompiler
import com.turbolent.questionCompiler.graph._
import com.turbolent.questionCompiler.sparql.SparqlGraphCompiler
import com.turbolent.questionParser.{ListParser, BaseParser, Token}
import junit.framework.TestCase
import org.apache.jena.query.{Query, QueryFactory}
import org.apache.jena.sparql.algebra.Algebra
import org.hamcrest.CoreMatchers.instanceOf
import org.hamcrest.Matcher
import org.hamcrest.MatcherAssert.assertThat
import org.junit.Assert._


class OntologyTest extends TestCase {

  implicit lazy val lemmatizer =
    Lemmatizer.loadFrom(Paths.get("lemmatizer-model"))

  def compileListQuestion(sentence: String) = {
    val tokens = tokenizeSentence(sentence)
    val result = ListParser.parse(tokens, ListParser.phrase(ListParser.Question))
    assertSuccess(result)
    val question = result.get
    new QuestionCompiler(WikidataOntology, new WikidataEnvironment())
        .compileQuestion(question)
  }

  def tokenizeSentence(taggedSentence: String) =
    taggedSentence.split(' ').toSeq map { taggedWord =>
      val Array(word, tag) = taggedWord.split('/')
      Token(word, tag)
    }

  def assertSuccess(x: Any) {
    val matcher: Matcher[Any] = instanceOf(classOf[BaseParser#Success[Any]])
    assertThat(x, matcher)
  }

  def assertEquivalent(expected: Query, actual: Query) {
    // NOTE: compare Ops, comparing queries is broken (ElementUnion)
    val expectedOp = Algebra.compile(expected)
    val actualOp = Algebra.compile(actual)
    try {
      assertEquals(expectedOp, actualOp)
    } catch {
      case e: Throwable =>
        println(actual)
        throw e
    }
  }

  val PROLOGUE = """
        |PREFIX  bd:        <http://www.bigdata.com/rdf#>
        |PREFIX  wdt:       <http://www.wikidata.org/prop/direct/>
        |PREFIX  wikibase:  <http://wikiba.se/ontology#>
        |PREFIX  xsd:       <http://www.w3.org/2001/XMLSchema#>
        |PREFIX  rdfs:      <http://www.w3.org/2000/01/rdf-schema#>
        |PREFIX  wd:        <http://www.wikidata.org/entity/>
        |PREFIX  p:         <http://www.wikidata.org/prop/>
        |PREFIX  v:         <http://www.wikidata.org/prop/statement/>
        |
      """.stripMargin

  val SELECT_FORMAT = "SELECT DISTINCT ?%s  ?%sLabel"
  val WHERE_FORMAT = """
      |WHERE {
      |  %s
      |  SERVICE wikibase:label {
      |     bd:serviceParam wikibase:language "en" .
      |  }
      |}
    """.stripMargin

  def parseSparqlQuery(variable: String, query: String) =
    QueryFactory.create(PROLOGUE
                        + String.format(SELECT_FORMAT, variable, variable)
                        + String.format(WHERE_FORMAT, query))

  def compileSparqlQuery(node: WikidataNode) =
    new SparqlGraphCompiler[NodeLabel, EdgeLabel](WikidataSparqlBackend)
        .compileQuery(node)

  def testQuestions() {
    {
      val actualNodes = compileListQuestion("who/WP acted/VBD in/IN Alien/NNP")

      // PersonListQuestion(PropertyWithFilter(List(Token("acted", "VBD")),
      //   FilterWithModifier(List(Token("in", "IN")), NamedValue(List(Token("Alien", "NNP"))))))

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val movie = env.newNode()
          .out(NameLabel, "Alien")

      val expectedNodes = List(person.in(movie, P.hasCastMember))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q5
          |  { ?2  wdt:P161    ?1 ;
          |        rdfs:label  "Alien"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("who/WP starred/VBD in/IN Inception/NNP")

      // PersonListQuestion(PropertyWithFilter(List(Token("starred", "VBD")),
      //   FilterWithModifier(List(Token("in", "IN")),
      //     NamedValue(List(Token("Inception", "NNP"))))))

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val movie = env.newNode()
          .out(NameLabel, "Inception")

      val expectedNodes = List(person.in(movie, P.hasCastMember))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q5
          |  { ?2  wdt:P161    ?1 ;
          |       rdfs:label  "Inception"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Movies/NNP starring/VB Winona/NNP Ryder/NNP")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("Movies", "NNP"))),
      //   PropertyWithFilter(List(Token("starring", "VB")),
      //     PlainFilter(NamedValue(List(Token("Winona", "NNP"), Token("Ryder", "NNP")))))))

      val env = new WikidataEnvironment()

      val movie = env.newNode().out(P.isA, Q.movie)

      val actress = env.newNode()
          .out(NameLabel, "Winona Ryder")

      val expectedNodes = List(movie.out(P.hasCastMember, actress))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q11424
          |  { ?1  wdt:P161    ?2 .
          |    ?2  rdfs:label  "Winona Ryder"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("In/IN what/WDT movies/NN "
                                            + "did/VBD Jennifer/NNP Aniston/NNP appear/VB")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("movies", "NN"))),
      //   InversePropertyWithFilter(List(Token("did", "VBD"), Token("appear", "VB")),
      //     PlainFilter(NamedValue(List(Token("Jennifer", "NNP"), Token("Aniston", "NNP")))))))

      val env = new WikidataEnvironment()

      val movie = env.newNode().out(P.isA, Q.movie)

      val actress = env.newNode()
          .out(NameLabel, "Jennifer Aniston")

      val expectedNodes = List(movie.out(P.hasCastMember, actress))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q11424
          |  { ?1  wdt:P161    ?2 .
          |    ?2  rdfs:label  "Jennifer Aniston"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("who/WP are/VBP the/DT actors/NNS "
                                            + "which/WDT starred/VBD in/IN Inception/NNP")
      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("the", "DT"),
      //   Token("actors", "NNS"))),
      //   PropertyWithFilter(List(Token("starred", "VBD")),
      //     FilterWithModifier(List(Token("in", "IN")),
      //       NamedValue(List(Token("Inception", "NNP")))))))

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.actor)
          .or(out(P.hasOccupation, Q.actor))

      val movie = env.newNode()
          .out(NameLabel, "Inception")

      val expectedNodes = List(person.in(movie, P.hasCastMember))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{   { ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q33999 }
          |  UNION
          |    { ?1 p:P106/(v:P106/(wdt:P279)*) wd:Q33999 }
          |  ?2  wdt:P161    ?1 ;
          |      rdfs:label  "Inception"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("who/WP directed/VBD Pocahontas/NNP")

      // PersonListQuestion(PropertyWithFilter(List(Token("directed", "VBD")),
      //   PlainFilter(NamedValue(List(Token("Pocahontas", "NNP"))))))

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val movie = env.newNode()
          .out(NameLabel, "Pocahontas")

      val expectedNodes = List(person.in(movie, P.hasDirector))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
          |  { ?2  wdt:P57     ?1 ;
          |        rdfs:label  "Pocahontas"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Who/WP did/VBD Bill/NNP Clinton/NNP marry/VB")

      // PersonListQuestion(InversePropertyWithFilter(List(Token("did", "VBD"),
      //   Token("marry", "VB")),
      //   PlainFilter(NamedValue(List(Token("Bill", "NNP"), Token("Clinton", "NNP"))))))

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val bill = env.newNode()
          .out(NameLabel, "Bill Clinton")

      val expectedNodes = List(person.out(P.hasSpouse, bill))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
          |  { ?1  wdt:P26     ?2 .
          |    ?2  rdfs:label  "Bill Clinton"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("which/WDT mountains/NNS "
                                            + "are/VBP 1000/CD meters/NNS high/JJ")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("mountains", "NNS"))),
      //   AdjectivePropertyWithFilter(List(Token("are", "VBP"), Token("high", "JJ")),
      //     PlainFilter(NumberWithUnit(List(Token("1000", "CD")), List(Token("meters", "NNS")))))))

      val env = new WikidataEnvironment()

      val elevation: WikidataNode = (1000.0, U.meter)

      val mountain = env.newNode()
          .out(P.isA, Q.mountain)
          .out(P.hasElevation, elevation)

      val expectedNodes = List(mountain)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q8502
          |  { ?1  wdt:P2044  "1000.0"^^xsd:integer }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("authors/NNS which/WDT died/VBD in/IN Berlin/NNP")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("authors", "NNS"))),
      //   PropertyWithFilter(List(Token("died", "VBD")),
      //     FilterWithModifier(List(Token("in", "IN")),
      //       NamedValue(List(Token("Berlin", "NNP")))))))

      val env = new WikidataEnvironment()

      val author = env.newNode()
          .in(env.newNode(), P.hasAuthor)

      val place = env.newNode()
          .out(NameLabel, "Berlin")

      val expectedNodes = List(author.out(P.hasPlaceOfDeath, place))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?2  wdt:P50  ?1
          |  { ?1 wdt:P20/(wdt:P131)* ?3
          |    { ?3  rdfs:label  "Berlin"@en }
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("authors/NNS which/WDT "
                                            + "were/VBD born/VBD in/IN Berlin/NNP "
                                            + "and/CC died/VBD in/IN Paris/NNP")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("authors", "NNS"))),
      //   AndProperty(List(PropertyWithFilter(List(Token("were", "VBD"),
      //     Token("born", "VBD")),FilterWithModifier(List(Token("in", "IN")),
      //     NamedValue(List(Token("Berlin", "NNP"))))),
      //     PropertyWithFilter(List(Token("died", "VBD")),
      //       FilterWithModifier(List(Token("in", "IN")),
      //         NamedValue(List(Token("Paris", "NNP")))))))))

      val env = new WikidataEnvironment()

      val author = env.newNode()
          .in(env.newNode(), P.hasAuthor)

      val place = env.newNode()
          .out(NameLabel, "Berlin")

      val place2 = env.newNode()
          .out(NameLabel, "Paris")

      val expectedNodes = List(author
          .out(P.hasPlaceOfBirth, place)
          .out(P.hasPlaceOfDeath, place2))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
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
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("who/WP was/VBD born/VBD in/IN Berlin/NNP "
                                            + "or/CC died/VBD in/IN Paris/NNP")

      // PersonListQuestion(OrProperty(List(PropertyWithFilter(List(Token("was", "VBD"),
      //   Token("born", "VBD")), FilterWithModifier(List(Token("in", "IN")),
      //   NamedValue(List(Token("Berlin", "NNP"))))),
      //   PropertyWithFilter(List(Token("died", "VBD")),
      //     FilterWithModifier(List(Token("in", "IN")),
      //       NamedValue(List(Token("Paris", "NNP"))))))))

      val env = new WikidataEnvironment()

      val author = env.newNode()
          .out(P.isA, Q.human)

      val place = env.newNode()
          .out(NameLabel, "Berlin")

      val place2 = env.newNode()
          .out(NameLabel, "Paris")

      val expectedNodes = List(author
          .and(out(P.hasPlaceOfBirth, place)
               or out(P.hasPlaceOfDeath, place2)))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
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
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Which/WDT presidents/NNS "
                                            + "were/VBD born/VBN before/IN 1900/CD")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("presidents", "NNS"))),
      //   PropertyWithFilter(List(Token("were", "VBD"), Token("born", "VBN")),
      //     FilterWithModifier(List(Token("before", "IN")), Number(List(Token("1900", "CD")))))))

      val env = new WikidataEnvironment()

      val president = env.newNode()
          .in(env.newNode(), P.hasHeadOfState)

      val year: WikidataNode = Year.of(1900)

      val date = env.newNode()
          .filter(LessThanFilter(year))

      val expectedNodes = List(president.out(P.hasDateOfBirth, date))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      // TODO: extract year from ?2
      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?2  wdt:P35  ?1
          |  { ?1  wdt:P569  ?3
          |    FILTER ( ?3 < 1900 )
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Give/VB me/PRP all/DT actors/NNS born/VBN in/IN "
                                            + "Berlin/NNP or/CC San/NNP Francisco/NNP")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("actors", "NNS"))),
      //   PropertyWithFilter(List(Token("born", "VBN")), FilterWithModifier(List(Token("in", "IN")),
      //     OrValue(List(NamedValue(List(Token("Berlin", "NNP"))),
      //       NamedValue(List(Token("San", "NNP"), Token("Francisco", "NNP")))))))))

      val env = new WikidataEnvironment()

      val actor = env.newNode()
          .out(P.isA, Q.actor)
          .or(out(P.hasOccupation, Q.actor))

      val place = env.newNode()
          .out(NameLabel, "Berlin")

      val place2 = env.newNode()
          .out(NameLabel, "San Francisco")

      val expectedNodes = List(actor
          .and(out(P.hasPlaceOfBirth, place)
               or out(P.hasPlaceOfBirth, place2)))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
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
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Which/WDT movies/NNS "
                                            + "were/VBD filmed/VBD in/IN Germany/NNP "
                                            + "and/CC Denmark/NNP or/CC California/NNP")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("movies", "NNS"))),
      //   PropertyWithFilter(List(Token("were", "VBD"), Token("filmed", "VBD")),
      //     FilterWithModifier(List(Token("in", "IN")),
      //       OrValue(List(AndValue(List(NamedValue(List(Token("Germany", "NNP"))),
      //         NamedValue(List(Token("Denmark", "NNP"))))),
      //         NamedValue(List(Token("California", "NNP")))))))))

      val env = new WikidataEnvironment()

      val movie = env.newNode()
          .out(P.isA, Q.movie)

      val germany = env.newNode()
          .out(NameLabel, "Germany")

      val denmark = env.newNode()
          .out(NameLabel, "Denmark")

      val california = env.newNode()
          .out(NameLabel, "California")

      val expectedNodes = List(movie
          .and((out(P.hasFilmingLocation, germany) and
                out(P.hasFilmingLocation, denmark))
               or out(P.hasFilmingLocation, california)))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
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
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Give/VB me/PRP all/DT musicians/NNS that/WDT "
                                            + "were/VBD born/VBN in/IN Vienna/NNP "
                                            + "and/CC died/VBN in/IN Berlin/NNP")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("musicians", "NNS"))),
      //   AndProperty(List(PropertyWithFilter(List(Token("were", "VBD"), Token("born", "VBN")),
      //     FilterWithModifier(List(Token("in", "IN")),
      //       NamedValue(List(Token("Vienna", "NNP"))))),
      //     PropertyWithFilter(List(Token("died", "VBN")),
      //       FilterWithModifier(List(Token("in", "IN")),
      //         NamedValue(List(Token("Berlin", "NNP")))))))))

      val env = new WikidataEnvironment()

      val musician = env.newNode()
          .out(P.isA, Q.musician)
          .or(out(P.hasOccupation, Q.musician))

      val place = env.newNode()
          .out(NameLabel, "Vienna")

      val place2 = env.newNode()
          .out(NameLabel, "Berlin")

      val expectedNodes = List(musician
          .and(out(P.hasPlaceOfBirth, place))
          .and(out(P.hasPlaceOfDeath, place2)))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
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
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Which/WDT books/NN "
                                            + "did/VBD George/NNP Orwell/NNP write/VB")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NN"))),
      //   InversePropertyWithFilter(List(Token("did", "VBD"), Token("write", "VB")),
      //     PlainFilter(NamedValue(List(Token("George", "NNP"), Token("Orwell", "NNP")))))))

      val env = new WikidataEnvironment()

      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expectedNodes = List(book.out(P.hasAuthor, author))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q571
          |  { ?1  wdt:P50     ?2 .
          |    ?2  rdfs:label  "George Orwell"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("list/VB presidents/NNS of/IN Argentina/NNP")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("presidents", "NNS"))),
      //   NamedQuery(List(Token("Argentina", "NNP"))),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      val country = env.newNode()
          .out(NameLabel, "Argentina")

      val person = env.newNode()
          .in(country, P.hasHeadOfState)

      val expectedNodes = List(person)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("2",
        """
          |{ ?1  wdt:P35     ?2 ;
          |      rdfs:label  "Argentina"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("List/VB albums/NNS of/IN Pink/NNP Floyd/NNP")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("albums", "NNS"))),
      //   NamedQuery(List(Token("Pink", "NNP"), Token("Floyd", "NNP"))),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      val artist = env.newNode()
          .out(NameLabel, "Pink Floyd")

      val album = env.newNode()
          .out(P.isA, Q.album)
          .out(P.hasPerformer, artist)

      val expectedNodes = List(album)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("2",
        """
          |{ ?2 p:P31/(v:P31/(wdt:P279)*) wd:Q482994
          |  { ?2  wdt:P175    ?1 .
          |    ?1  rdfs:label  "Pink Floyd"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("List/VB the/DT actors/NNS of/IN Titanic/NNP")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("actors", "NNS"))),
      //   NamedQuery(List(Token("Titanic", "NNP"))),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      val movie = env.newNode()
          .out(NameLabel, "Titanic")

      val actor = env.newNode()
          .out(P.hasOccupation, Q.actor)
          .and(in(movie, P.hasCastMember))

      val expectedNodes = List(actor)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("2",
        """
          |{ ?2 p:P106/(v:P106/(wdt:P279)*) wd:Q33999
          |  { ?1  wdt:P161    ?2 ;
          |        rdfs:label  "Titanic"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Who/WP is/VBZ the/DT director/NN of/IN Big/NN Fish/NN")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("director", "NN"))),
      //   NamedQuery(List(Token("Big", "NN"), Token("Fish", "NN"))),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      val movie = env.newNode()
          .out(NameLabel, "Big Fish")

      val director = env.newNode()
          .out(P.isA, Q.filmDirector)
          .in(movie, P.hasDirector)

      val expectedNodes = List(director)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("2",
        """
          |{ ?2 p:P31/(v:P31/(wdt:P279)*) wd:Q2526255
          |  { ?1  wdt:P57     ?2 ;
          |        rdfs:label  "Big Fish"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("What/WP are/VBP the/DT members/NNS "
                                            + "of/IN Metallica/NNP")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("members", "NNS"))),
      //   NamedQuery(List(Token("Metallica", "NNP"))),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      val band = env.newNode()
          .out(NameLabel, "Metallica")

      val member = env.newNode()
          .out(P.isMemberOf, band)

      val expectedNodes = List(member)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("2",
        """
          |{ ?2  wdt:P463    ?1 .
          |  ?1  rdfs:label  "Metallica"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("members/NNS of/IN Metallica/NNP")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("members", "NNS"))),
      //   NamedQuery(List(Token("Metallica", "NNP"))),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      val band = env.newNode()
          .out(NameLabel, "Metallica")

      val member = env.newNode()
          .out(P.isMemberOf, band)

      val expectedNodes = List(member)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("2",
        """
          |{ ?2  wdt:P463    ?1 .
          |  ?1  rdfs:label  "Metallica"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("What/WP is/VBZ the/DT music/NN genre/NN "
                                            + "of/IN Gorillaz/NNP")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("music", "NN"), Token("genre", "NN"))),
      //   NamedQuery(List(Token("Gorillaz", "NNP"))),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      val band = env.newNode()
          .out(NameLabel, "Gorillaz")

      val genre = env.newNode()
          .out(P.isA, Q.musicGenre)
          .in(band, P.hasGenre)

      val expectedNodes = List(genre)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("2",
        """
          |{ ?2 p:P31/(v:P31/(wdt:P279)*) wd:Q189991
          |  { ?1  wdt:P136    ?2 ;
          |        rdfs:label  "Gorillaz"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("What/WP is/VBZ the/DT cast/NN of/IN Friends/NNS")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"), Token("cast", "NN"))),
      //   NamedQuery(List(Token("Friends", "NNS"))),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      val show = env.newNode()
          .out(NameLabel, "Friends")

      val member = env.newNode()
          .in(show, P.hasCastMember)

      val expectedNodes = List(member)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("2",
        """
          |{ ?1  wdt:P161    ?2 ;
          |      rdfs:label  "Friends"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("who/WP are/VBP the/DT children/NNS "
                                            + "of/IN the/DT presidents/NNS")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("children", "NNS"))),
      //   NamedQuery(List(Token("the", "DT"), Token("presidents", "NNS"))),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      val president = env.newNode()
          .in(env.newNode(), P.hasHeadOfState)

      val child = env.newNode()
          .in(president, P.hasChild)

      val expectedNodes = List(child)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("3",
        """
          |{ ?1  wdt:P40  ?3 .
          |  ?2  wdt:P35  ?1
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("what/WP are/VBP the/DT population/NN sizes/NNS "
                                            + "of/IN cities/NNS "
                                            + "located/VBN in/IN california/NN")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("population", "NN"),
      //   Token("sizes", "NNS"))),
      //   QueryWithProperty(NamedQuery(List(Token("cities", "NNS"))),
      //     PropertyWithFilter(List(Token("located", "VBN")),
      //       FilterWithModifier(List(Token("in", "IN")),
      //         NamedValue(List(Token("california", "NN")))))),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      val city = env.newNode()
          .out(P.isA, Q.city)
          .out(P.isLocatedIn,
            env.newNode()
                .out(NameLabel, "california"))

      val population = env.newNode()
          .in(city, P.hasPopulation)

      val expectedNodes = List(population)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("3",
        """
          |{ ?1  wdt:P1082  ?3
          |  { ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q515
          |    { ?1 (wdt:P131)+ ?2
          |      { ?2  rdfs:label  "california"@en }
          |    }
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Who/WP are/VBP the/DT children/NNS "
                                            + "of/IN the/DT children/NNS "
                                            + "of/IN Bill/NNP Clinton/NNP")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("children", "NNS"))),
      //   RelationshipQuery(NamedQuery(List(Token("the", "DT"), Token("children", "NNS"))),
      //     NamedQuery(List(Token("Bill", "NNP"), Token("Clinton", "NNP"))),
      //     Token("of", "IN")),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      val bill = env.newNode()
          .out(NameLabel, "Bill Clinton")

      val child = env.newNode()
          .in(bill, P.hasChild)

      val grandChild = env.newNode()
          .in(child, P.hasChild)

      val expectedNodes = List(grandChild)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("3",
        """
          |{ ?2  wdt:P40     ?3 .
          |  ?1  wdt:P40     ?2 ;
          |      rdfs:label  "Bill Clinton"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Clinton/NNP 's/POS daughters/NN")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("daughters", "NN"))),
      //   NamedQuery(List(Token("Clinton", "NNP"))),
      //   Token("'s", "POS")))

      val env = new WikidataEnvironment()

      val bill = env.newNode()
          .out(NameLabel, "Clinton")

      val child = env.newNode()
          .out(P.hasGender, Q.female)
          .in(bill, P.hasChild)

      val expectedNodes = List(child)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("2",
        """
          |{ ?2  wdt:P21     wd:Q6581072 .
          |  ?1  wdt:P40     ?2 ;
          |      rdfs:label  "Clinton"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("What/WP are/VBP the/DT largest/JJS cities/NNS "
                                            + "of/IN California/NNP")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("largest", "JJS"), Token("cities", "NNS"))),
      //   NamedQuery(List(Token("California", "NNP"))),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      val state = env.newNode()
          .out(NameLabel, "California")

      val population = env.newNode()
          .aggregate(Max)

      val city = env.newNode()
          .out(P.isA, Q.city)
          .out(P.isLocatedIn, state)
          .out(P.hasPopulation, population)

      val expectedNodes = List(city)

      assertEquals(expectedNodes, actualNodes)

      // TODO: implement aggregates in SPARQL compiler
    }
    {
      val actualNodes = compileListQuestion("What/WP are/VBP the/DT biggest/JJS cities/NNS "
                                            + "of/IN California/NNP")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("biggest", "JJS"), Token("cities", "NNS"))),
      //   NamedQuery(List(Token("California", "NNP"))),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      val state = env.newNode()
          .out(NameLabel, "California")

      val population = env.newNode()
          .aggregate(Max)

      val city = env.newNode()
          .out(P.isA, Q.city)
          .out(P.isLocatedIn, state)
          .out(P.hasPopulation, population)

      val expectedNodes = List(city)

      assertEquals(expectedNodes, actualNodes)

      // TODO: implement aggregates in SPARQL compiler
    }
    {
      val actualNodes = compileListQuestion("What/WP are/VBP California/NNP 's/POS "
                                            + "biggest/JJS cities/NNS")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("biggest", "JJS"),
      //   Token("cities", "NNS"))),
      //   NamedQuery(List(Token("California", "NNP"))),
      //   Token("'s", "POS")))

      val env = new WikidataEnvironment()

      val state = env.newNode()
          .out(NameLabel, "California")

      val population = env.newNode()
          .aggregate(Max)

      val city = env.newNode()
          .out(P.isA, Q.city)
          .out(P.isLocatedIn, state)
          .out(P.hasPopulation, population)

      val expectedNodes = List(city)

      assertEquals(expectedNodes, actualNodes)

      // TODO: implement aggregates in SPARQL compiler
    }
    {
      val actualNodes = compileListQuestion("Who/WP is/VBZ Bill/NNP Clinton/NNP 's/POS daughter/NN")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("daughter", "NN"))),
      //   NamedQuery(List(Token("Bill", "NNP"), Token("Clinton", "NNP"))),
      //   Token("'s", "POS")))

      val env = new WikidataEnvironment()

      val bill = env.newNode()
          .out(NameLabel, "Bill Clinton")

      val daughter = env.newNode()
          .out(P.hasGender, Q.female)
          .in(bill, P.hasChild)

      val expectedNodes = List(daughter)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("2",
        """
          |{ ?2  wdt:P21     wd:Q6581072 .
          |  ?1  wdt:P40     ?2 ;
          |      rdfs:label  "Bill Clinton"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Who/WP is/VBZ Bill/NNP Clinton/NNP 's/POS "
                                            + "daughter/NN 's/POS "
                                            + "husband/NN 's/POS "
                                            + "daughter/NN 's/POS "
                                            + "grandfather/NN")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("grandfather", "NN"))),
      //   RelationshipQuery(NamedQuery(List(Token("daughter", "NN"))),
      //     RelationshipQuery(NamedQuery(List(Token("husband", "NN"))),
      //       RelationshipQuery(NamedQuery(List(Token("daughter", "NN"))),
      //         NamedQuery(List(Token("Bill", "NNP"), Token("Clinton", "NNP"))),
      //         Token("'s", "POS")),
      //       Token("'s", "POS")),
      //     Token("'s", "POS")),
      //   Token("'s", "POS")))

      val env = new WikidataEnvironment()

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

      val expectedNodes = List(grandFather)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("6",
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
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Who/WP are/VBP the/DT daughters/NNS "
                                            + "of/IN the/DT wife/NN "
                                            + "of/IN the/DT president/NN "
                                            + "of/IN the/DT United/NNP States/NNPS")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("daughters", "NNS"))),
      //   RelationshipQuery(NamedQuery(List(Token("the", "DT"), Token("wife", "NN"))),
      //     RelationshipQuery(NamedQuery(List(Token("the", "DT"), Token("president", "NN"))),
      //       NamedQuery(List(Token("the", "DT"),
      //         Token("United", "NNP"), Token("States", "NNPS"))),
      //       Token("of", "IN")),
      //     Token("of", "IN")),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

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

      val expectedNodes = List(daughter)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("4",
        """
          |{ ?4  wdt:P21     wd:Q6581072 .
          |  ?3  wdt:P40     ?4 ;
          |      wdt:P21     wd:Q6581072 .
          |  ?2  wdt:P26     ?3 .
          |  ?1  wdt:P35     ?2 ;
          |      rdfs:label  "the United States"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Who/WP is/VBZ the/DT son/NN "
                                            + "of/IN the/DT actor/NN "
                                            + "of/IN ``/`` I/PRP ,/, Robot/NNP ''/''")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"), Token("son", "NN"))),
      //   RelationshipQuery(NamedQuery(List(Token("the", "DT"), Token("actor", "NN"))),
      //     NamedQuery(List(Token("I", "PRP"), Token(",", ","), Token("Robot", "NNP"))),
      //     Token("of", "IN")),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      // TODO: whitespace should be preserved (no space between 'I' and comma)
      val movie = env.newNode()
          .out(NameLabel, "I , Robot")

      val actor = env.newNode()
          .out(P.hasOccupation, Q.actor)
          .in(movie, P.hasCastMember)

      val son = env.newNode()
          .out(P.hasGender, Q.male)
          .in(actor, P.hasChild)

      val expectedNodes = List(son)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("3",
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
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("children/NNS of/IN all/DT presidents/NNS "
                                            + "of/IN the/DT US/NNP")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("children", "NNS"))),
      //   RelationshipQuery(NamedQuery(List(Token("all", "DT"), Token("presidents", "NNS"))),
      //     NamedQuery(List(Token("the", "DT"), Token("US", "NNP"))),
      //     Token("of", "IN")),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      val country = env.newNode()
          .out(NameLabel, "the US")

      val president = env.newNode()
          .in(country, P.hasHeadOfState)

      val child = env.newNode()
          .in(president, P.hasChild)

      val expectedNodes = List(child)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("3",
        """
          |{ ?2  wdt:P40     ?3 .
          |  ?1  wdt:P35     ?2 ;
          |      rdfs:label  "the US"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("List/VB movies/NNS "
                                            + "directed/VBN by/IN Quentin/NNP Tarantino/NNP")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("movies", "NNS"))),
      //   PropertyWithFilter(List(Token("directed", "VBN")),
      //     FilterWithModifier(List(Token("by", "IN")),
      //       NamedValue(List(Token("Quentin", "NNP"), Token("Tarantino", "NNP")))))))

      val env = new WikidataEnvironment()

      val movie = env.newNode()
          .out(P.isA, Q.movie)

      val director = env.newNode()
          .out(NameLabel, "Quentin Tarantino")

      val expectedNodes = List(movie.out(P.hasDirector, director))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q11424
          |  { ?1  wdt:P57     ?2 .
          |    ?2  rdfs:label  "Quentin Tarantino"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Which/WDT movies/NN "
                                            + "did/VBD Mel/NNP Gibson/NNP direct/VB")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("movies", "NN"))),
      //   InversePropertyWithFilter(List(Token("did", "VBD"), Token("direct", "VB")),
      //     PlainFilter(NamedValue(List(Token("Mel", "NNP"), Token("Gibson", "NNP")))))))

      val env = new WikidataEnvironment()

      val movie = env.newNode()
          .out(P.isA, Q.movie)

      val director = env.newNode()
          .out(NameLabel, "Mel Gibson")

      val expectedNodes = List(movie.out(P.hasDirector, director))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q11424
          |  { ?1  wdt:P57     ?2 .
          |    ?2  rdfs:label  "Mel Gibson"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("actors/NNP")

      // ListQuestion(NamedQuery(List(Token("actors", "NNP"))))

      val env = new WikidataEnvironment()

      val actor = env.newNode()
          .out(P.isA, Q.actor)
          .or(out(P.hasOccupation, Q.actor))

      val expectedNodes = List(actor)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{   { ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q33999 }
          |  UNION
          |    { ?1 p:P106/(v:P106/(wdt:P279)*) wd:Q33999 }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("what/WDT languages/NNS "
                                            + "are/VBP spoken/VBN in/IN Switzerland/NNP")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("languages", "NNS"))),
      //   PropertyWithFilter(List(Token("are", "VBP"), Token("spoken", "VBN")),
      //     FilterWithModifier(List(Token("in", "IN")),
      //       NamedValue(List(Token("Switzerland", "NNP")))))))

      val env = new WikidataEnvironment()

      val language = env.newNode()
          .out(P.isA, Q.language)

      val country = env.newNode()
          .out(NameLabel, "Switzerland")

      val expectedNodes = List(language
          .in(country, P.hasOfficialLanguage))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q34770
          |  { ?2  wdt:P37     ?1 ;
          |        rdfs:label  "Switzerland"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Which/WDT books/NN "
                                            + "did/VBD Orwell/NNP or/CC Shakespeare/NNP write/VB")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NN"))),
      //   InversePropertyWithFilter(List(Token("did", "VBD"), Token("write", "VB")),
      //     PlainFilter(OrValue(List(NamedValue(List(Token("Orwell", "NNP"))),
      //       NamedValue(List(Token("Shakespeare", "NNP")))))))))


      val env = new WikidataEnvironment()

      val book = env.newNode()
          .out(P.isA, Q.book)

      val orwell = env.newNode()
          .out(NameLabel, "Orwell")

      val shakespeare = env.newNode()
          .out(NameLabel, "Shakespeare")

      val expectedNodes = List(book
          .and(out(P.hasAuthor, orwell)
               or out(P.hasAuthor, shakespeare)))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
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
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Which/WDT books/NN "
                                            + "were/VBD authored/VBN by/IN George/NNP Orwell/NNP")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NN"))),
      //   PropertyWithFilter(List(Token("were", "VBD"), Token("authored", "VBN")),
      //     FilterWithModifier(List(Token("by", "IN")),
      //       NamedValue(List(Token("George", "NNP"), Token("Orwell", "NNP")))))))

      val env = new WikidataEnvironment()

      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expectedNodes = List(book
          .out(P.hasAuthor, author))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q571
          |  { ?1  wdt:P50     ?2 .
          |    ?2  rdfs:label  "George Orwell"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Which/WDT instrument/NN "
                                            + "did/VBD John/NNP Lennon/NNP play/VB")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("instrument", "NN"))),
      //   InversePropertyWithFilter(List(Token("did", "VBD"), Token("play", "VB")),
      //     PlainFilter(NamedValue(List(Token("John", "NNP"), Token("Lennon", "NNP")))))))

      val env = new WikidataEnvironment()

      val instrument = env.newNode()
          .out(P.isA, Q.musicalInstrument)

      val musician = env.newNode()
          .out(NameLabel, "John Lennon")

      val expectedNodes = List(instrument
          .in(musician, P.playsInstrument))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q34379
          |  { ?2  wdt:P1303   ?1 ;
          |        rdfs:label  "John Lennon"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Who/WP wrote/VBD "
                                            + "``/`` Le/NNP Petit/NNP Prince/NNP ''/'' "
                                            + "and/CC ``/`` Vol/NNP de/IN Nuit/NNP ''/''")

      // PersonListQuestion(PropertyWithFilter(List(Token("wrote", "VBD")),
      //   PlainFilter(AndValue(List(NamedValue(List(Token("Le", "NNP"),
      //     Token("Petit", "NNP"), Token("Prince", "NNP"))),
      //     NamedValue(List(Token("Vol", "NNP"), Token("de", "IN"), Token("Nuit", "NNP"))))))))

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val book = env.newNode()
          .out(NameLabel, "Le Petit Prince")

      val book2 = env.newNode()
          .out(NameLabel, "Vol de Nuit")

      val expectedNodes = List(person
          .and(in(book, P.hasAuthor))
          .and(in(book2, P.hasAuthor)))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ { ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
          |    { ?2  wdt:P50     ?1 ;
          |          rdfs:label  "Le Petit Prince"@en
          |    }
          |  }
          |  ?3  wdt:P50     ?1 ;
          |      rdfs:label  "Vol de Nuit"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("What/WP did/VBD George/NNP Orwell/NNP write/VB")

      // ThingListQuestion(InversePropertyWithFilter(List(Token("did", "VBD"),
      //   Token("write", "VB")),
      //   PlainFilter(NamedValue(List(Token("George", "NNP"), Token("Orwell", "NNP"))))))

      val env = new WikidataEnvironment()

      val thing = env.newNode()

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expectedNodes = List(thing
          .out(P.hasAuthor, author))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1  wdt:P50     ?2 .
          |  ?2  rdfs:label  "George Orwell"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("What/WP was/VBD authored/VBN "
                                            + "by/IN George/NNP Orwell/NNP")

      // ThingListQuestion(PropertyWithFilter(List(Token("was", "VBD"), Token("authored", "VBN")),
      //   FilterWithModifier(List(Token("by", "IN")),
      //     NamedValue(List(Token("George", "NNP"), Token("Orwell", "NNP"))))))

      val env = new WikidataEnvironment()

      val thing = env.newNode()

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expectedNodes = List(thing
          .out(P.hasAuthor, author))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1  wdt:P50     ?2 .
          |  ?2  rdfs:label  "George Orwell"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("What/WP books/NNP "
                                            + "were/VBD authored/VBN by/IN George/NNP Orwell/NNP")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NNP"))),
      //   PropertyWithFilter(List(Token("were", "VBD"), Token("authored", "VBN")),
      //     FilterWithModifier(List(Token("by", "IN")),
      //       NamedValue(List(Token("George", "NNP"), Token("Orwell", "NNP")))))))

      val env = new WikidataEnvironment()

      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expectedNodes = List(book
          .out(P.hasAuthor, author))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q571
          |  { ?1  wdt:P50     ?2 .
          |    ?2  rdfs:label  "George Orwell"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("authors/NNS who/WP died/VBD")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("authors", "NNS"))),
      //   NamedProperty(List(Token("died", "VBD")))))

      val env = new WikidataEnvironment()

      val author = env.newNode()
          .in(env.newNode(), P.hasAuthor)

      val place = env.newNode()

      val expectedNodes = List(author
          .and(out(P.hasDateOfDeath, place)
               or out(P.hasPlaceOfDeath, place)))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?2  wdt:P50  ?1
          |    { ?1  wdt:P570  ?3 }
          |  UNION
          |    { ?1  wdt:P20/(wdt:P131)*  ?3 }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("authors/NNS which/WDT died/VBD in/IN Berlin/NNP")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("authors", "NNS"))),
      //   PropertyWithFilter(List(Token("died", "VBD")),
      //     FilterWithModifier(List(Token("in", "IN")),
      //       NamedValue(List(Token("Berlin", "NNP")))))))

      val env = new WikidataEnvironment()

      val author = env.newNode()
          .in(env.newNode(), P.hasAuthor)

      val place = env.newNode()
          .out(NameLabel, "Berlin")

      val expectedNodes = List(author
          .out(P.hasPlaceOfDeath, place))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?2  wdt:P50  ?1
          |  { ?1 wdt:P20/(wdt:P131)* ?3
          |    { ?3  rdfs:label  "Berlin"@en }
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("What/WDT actor/NN "
                                            + "married/VBD John/NNP F./NNP Kennedy/NNP 's/POS "
                                            + "sister/NN")

      //        ListQuestion(QueryWithProperty(NamedQuery(List(Token("actor", "NN"))),
      //          PropertyWithFilter(List(Token("married", "VBD")),
      //            PlainFilter(ValueRelationship(NamedValue(List(Token("sister", "NN"))),
      //              NamedValue(List(Token("John", "NNP"), Token("F.", "NNP"),
      //                Token("Kennedy", "NNP"))))))))

      val env = new WikidataEnvironment()

      val actor = env.newNode()
          .out(P.isA, Q.actor)
          .or(out(P.hasOccupation, Q.actor))

      val kennedy = env.newNode()
          .out(NameLabel, "John F. Kennedy")

      val sister = env.newNode()
          .in(kennedy, P.hasSister)

      val expectedNodes = List(actor.out(P.hasSpouse, sister))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{   { ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q33999 }
          |  UNION
          |    { ?1 p:P106/(v:P106/(wdt:P279)*) wd:Q33999 }
          |  ?1  wdt:P26     ?3 .
          |  ?2  wdt:P9      ?3 ;
          |      rdfs:label  "John F. Kennedy"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Who/WP did/VBD Bill/NNP Clinton/NNP 's/POS "
                                            + "daughter/NN marry/VB")

      // PersonListQuestion(InversePropertyWithFilter(List(Token("did", "VBD"),
      //   Token("marry", "VB")),
      //   PlainFilter(ValueRelationship(NamedValue(List(Token("daughter", "NN"))),
      //     NamedValue(List(Token("Bill", "NNP"), Token("Clinton", "NNP")))))))

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val bill = env.newNode()
          .out(NameLabel, "Bill Clinton")

      val daughter = env.newNode()
          .out(P.hasGender, Q.female)
          .in(bill, P.hasChild)

      val expectedNodes = List(person.out(P.hasSpouse, daughter))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
          |  { ?1  wdt:P26     ?3 .
          |    ?3  wdt:P21     wd:Q6581072 .
          |    ?2  wdt:P40     ?3 ;
          |        rdfs:label  "Bill Clinton"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Clinton/NNP 's/POS "
                                            + "children/NNS and/CC grandchildren/NNS")

      // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("children", "NNS"))),
      //   NamedQuery(List(Token("grandchildren", "NNS"))))),
      //   NamedQuery(List(Token("Clinton", "NNP"))),
      //   Token("'s", "POS")))

      val env = new WikidataEnvironment()

      val bill = env.newNode()
          .out(NameLabel, "Clinton")

      val child = env.newNode()
          .in(bill, P.hasChild)
      // NOTE: another child node, but might be child
      val child2 = env.newNode()
          .in(bill, P.hasChild)

      val grandchild = env.newNode()
          .in(child2, P.hasChild)

      val expectedNodes = List(child, grandchild)

      assertEquals(expectedNodes, actualNodes)

      {
        val actualQuery = compileSparqlQuery(expectedNodes.head)

        val expectedQuery = parseSparqlQuery("2",
          """
            |{ ?1  wdt:P40     ?2 ;
            |      rdfs:label  "Clinton"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(1))

        val expectedQuery = parseSparqlQuery("4",
          """
            |{ ?3  wdt:P40     ?4 .
            |  ?1  wdt:P40     ?3 ;
            |      rdfs:label  "Clinton"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
    }
    {
      val actualNodes = compileListQuestion("What/WP are/VBP the/DT population/NN "
                                            + "of/IN China/NNP and/CC the/DT USA/NNP")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("population", "NN"))),
      //   AndQuery(List(NamedQuery(List(Token("China", "NNP"))),
      //     NamedQuery(List(Token("the", "DT"), Token("USA", "NNP"))))),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      val china = env.newNode()
          .out(NameLabel, "China")

      val usa = env.newNode()
          .out(NameLabel, "the USA")

      val populationOfChina = env.newNode().in(china, P.hasPopulation)

      val populationOfUSA = env.newNode().in(usa, P.hasPopulation)

      val expectedNodes = List(populationOfChina, populationOfUSA)

      assertEquals(expectedNodes, actualNodes)

      {
        val actualQuery = compileSparqlQuery(expectedNodes.head)

        val expectedQuery = parseSparqlQuery("3",
          """
            |{ ?1  wdt:P1082   ?3 ;
            |      rdfs:label  "China"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(1))

        val expectedQuery = parseSparqlQuery("4",
          """
            |{ ?2  wdt:P1082   ?4 ;
            |      rdfs:label  "the USA"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
    }
    {
      val actualNodes = compileListQuestion("the/DT population/NNP of/IN Japan/NNP and/CC China/NNP")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("population", "NNP"))),
      //   AndQuery(List(NamedQuery(List(Token("Japan", "NNP"))),
      //     NamedQuery(List(Token("China", "NNP"))))),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      val japan = env.newNode()
          .out(NameLabel, "Japan")

      val china = env.newNode()
          .out(NameLabel, "China")

      val populationOfJapan = env.newNode().in(japan, P.hasPopulation)

      val populationOfChina = env.newNode().in(china, P.hasPopulation)

      val expectedNodes = List(populationOfJapan, populationOfChina)

      assertEquals(expectedNodes, actualNodes)

      {
        val actualQuery = compileSparqlQuery(expectedNodes.head)

        val expectedQuery = parseSparqlQuery("3",
          """
            |{ ?1  wdt:P1082   ?3 ;
            |      rdfs:label  "Japan"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(1))

        val expectedQuery = parseSparqlQuery("4",
          """
            |{ ?2  wdt:P1082   ?4 ;
            |      rdfs:label  "China"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
    }
    {
      val actualNodes = compileListQuestion("the/DT population/NNP and/CC area/NNP "
                                            + "of/IN Japan/NNP and/CC China/NNP")

      // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("the", "DT"),
      //   Token("population", "NNP"))),
      //   NamedQuery(List(Token("area", "NNP"))))),
      //   AndQuery(List(NamedQuery(List(Token("Japan", "NNP"))),
      //     NamedQuery(List(Token("China", "NNP"))))),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

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

      val expectedNodes = List(populationOfJapan, populationOfChina,
        areaOfJapan, areaOfChina)

      assertEquals(expectedNodes, actualNodes)

      {
        val actualQuery = compileSparqlQuery(expectedNodes.head)

        val expectedQuery = parseSparqlQuery("3",
          """
            |{ ?1  wdt:P1082   ?3 ;
            |      rdfs:label  "Japan"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(1))

        val expectedQuery = parseSparqlQuery("4",
          """
            |{ ?2  wdt:P1082   ?4 ;
            |      rdfs:label  "China"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(2))

        val expectedQuery = parseSparqlQuery("5",
          """
            |{ ?1  wdt:P2046   ?5 ;
            |      rdfs:label  "Japan"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(3))

        val expectedQuery = parseSparqlQuery("6",
          """
            |{ ?2  wdt:P2046   ?6 ;
            |      rdfs:label  "China"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
    }
    {
      val actualNodes = compileListQuestion("the/DT population/NN "
                                            + ",/, land/NN area/NN "
                                            + "and/CC capitals/NNP "
                                            + "of/IN Japan/NNP "
                                            + ",/, India/NNP "
                                            + "and/CC China/NNP")

      // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("the", "DT"),
      //   Token("population", "NN"))),
      //   NamedQuery(List(Token("land", "NN"), Token("area", "NN"))),
      //   NamedQuery(List(Token("capitals", "NNP"))))),
      //   AndQuery(List(NamedQuery(List(Token("Japan", "NNP"))),
      //     NamedQuery(List(Token("India", "NNP"))),
      //     NamedQuery(List(Token("China", "NNP"))))),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

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

      val expectedNodes = List(populationOfJapan, populationOfIndia, populationOfChina,
        areaOfJapan, areaOfIndia, areaOfChina,
        capitalOfJapan, capitalOfIndia, capitalOfChina)

      assertEquals(expectedNodes, actualNodes)

      {
        val actualQuery = compileSparqlQuery(expectedNodes.head)

        val expectedQuery = parseSparqlQuery("4",
          """
            |{ ?1  wdt:P1082   ?4 ;
            |      rdfs:label  "Japan"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(1))

        val expectedQuery = parseSparqlQuery("5",
          """
            |{ ?2  wdt:P1082   ?5 ;
            |      rdfs:label  "India"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(2))

        val expectedQuery = parseSparqlQuery("6",
          """
            |{ ?3  wdt:P1082   ?6 ;
            |      rdfs:label  "China"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(3))

        val expectedQuery = parseSparqlQuery("7",
          """
            |{ ?1  wdt:P2046   ?7 ;
            |      rdfs:label  "Japan"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(4))

        val expectedQuery = parseSparqlQuery("8",
          """
            |{ ?2  wdt:P2046   ?8 ;
            |      rdfs:label  "India"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(5))

        val expectedQuery = parseSparqlQuery("9",
          """
            |{ ?3  wdt:P2046   ?9 ;
            |      rdfs:label  "China"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(6))

        val expectedQuery = parseSparqlQuery("10",
          """
            |{ ?1  wdt:P36     ?10 ;
            |      rdfs:label  "Japan"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(7))

        val expectedQuery = parseSparqlQuery("11",
          """
            |{ ?2  wdt:P36     ?11 ;
            |      rdfs:label  "India"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(8))

        val expectedQuery = parseSparqlQuery("12",
          """
            |{ ?3  wdt:P36     ?12 ;
            |      rdfs:label  "China"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
    }
    {
      val actualNodes = compileListQuestion("Japan/NNP and/CC China/NNP 's/POS "
                                            + "population/NNP and/CC area/NNP")

      // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("population", "NNP"))),
      //   NamedQuery(List(Token("area", "NNP"))))),
      //   AndQuery(List(NamedQuery(List(Token("Japan", "NNP"))),
      //     NamedQuery(List(Token("China", "NNP"))))),
      //   Token("'s", "POS")))

      val env = new WikidataEnvironment()

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

      val expectedNodes = List(populationOfJapan, populationOfChina,
        areaOfJapan, areaOfChina)

      assertEquals(expectedNodes, actualNodes)

      {
        val actualQuery = compileSparqlQuery(expectedNodes.head)

        val expectedQuery = parseSparqlQuery("3",
          """
            |{ ?1  wdt:P1082   ?3 ;
            |      rdfs:label  "Japan"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(1))

        val expectedQuery = parseSparqlQuery("4",
          """
            |{ ?2  wdt:P1082   ?4 ;
            |      rdfs:label  "China"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(2))

        val expectedQuery = parseSparqlQuery("5",
          """
            |{ ?1  wdt:P2046   ?5 ;
            |      rdfs:label  "Japan"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(3))

        val expectedQuery = parseSparqlQuery("6",
          """
            |{ ?2  wdt:P2046   ?6 ;
            |      rdfs:label  "China"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
    }
    {
      val actualNodes = compileListQuestion("children/NNS and/CC grandchildren/NNS "
                                            + "of/IN Clinton/NNP")

      // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("children", "NNS"))),
      //   NamedQuery(List(Token("grandchildren", "NNS"))))),
      //   NamedQuery(List(Token("Clinton", "NNP"))),
      //   Token("of", "IN")))

      val env = new WikidataEnvironment()

      val bill = env.newNode()
          .out(NameLabel, "Clinton")

      val child = env.newNode()
          .in(bill, P.hasChild)
      // NOTE: another child node, but might be child
      val child2 = env.newNode()
          .in(bill, P.hasChild)

      val grandchild = env.newNode()
          .in(child2, P.hasChild)

      val expectedNodes = List(child, grandchild)

      assertEquals(expectedNodes, actualNodes)

      {
        val actualQuery = compileSparqlQuery(expectedNodes.head)

        val expectedQuery = parseSparqlQuery("2",
          """
            |{ ?1  wdt:P40     ?2 ;
            |      rdfs:label  "Clinton"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(1))

        val expectedQuery = parseSparqlQuery("4",
          """
            |{ ?3  wdt:P40     ?4 .
            |  ?1  wdt:P40     ?3 ;
            |      rdfs:label  "Clinton"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
    }
    {
      val actualNodes = compileListQuestion("Who/WP wrote/VBD books/NNS")

      // PersonListQuestion(PropertyWithFilter(List(Token("wrote", "VBD")),
      //     PlainFilter(NamedValue(List(Token("books", "NNS"))))))

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val book = env.newNode()
          .out(P.isA, Q.book)

      val expectedNodes = List(person.in(book, P.hasAuthor))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
          |  { ?2  wdt:P50  ?1
          |    { ?2 p:P31/(v:P31/(wdt:P279)*) wd:Q571 }
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("presidents/NNS that/WDT have/VBP children/NNS")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("presidents", "NNS"))),
      //   PropertyWithFilter(List(Token("have", "VBP")),
      //     PlainFilter(NamedValue(List(Token("children", "NNS")))))

      val env = new WikidataEnvironment()

      val president = env.newNode()
          .in(env.newNode(), P.hasHeadOfState)

      val child = env.newNode()
          .in(env.newNode(), P.hasChild)

      val expectedNodes = List(president.out(P.hasChild, child))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?2  wdt:P35  ?1 .
          |  ?1  wdt:P40  ?3 .
          |  ?4  wdt:P40  ?3
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("what/WP are/VBP the/DT largest/JJS cities/NNS "
                                            + "in/IN europe/NN")

      // ListQuestion(QueryWithProperty(
      //   NamedQuery(List(Token("the", "DT"), Token("largest", "JJS"), Token("cities", "NNS"))),
      //   PropertyWithFilter(List(),
      //     FilterWithModifier(List(Token("in", "IN")),
      //       NamedValue(List(Token("europe", "NN")))))))

      val env = new WikidataEnvironment()

      val city = env.newNode()
          .out(P.isA, Q.city)

      val population = env.newNode()
          .aggregate(Max)

      val europe = env.newNode()
          .out(NameLabel, "europe")

      val expectedNodes = List(city
          .out(P.hasPopulation, population)
          .out(P.isLocatedIn, europe))

      assertEquals(expectedNodes, actualNodes)

      // TODO: implement aggregates in SPARQL compiler
    }
    {
      val actualNodes = compileListQuestion("List/VB books/NNS by/IN George/NNP Orwell/NNP")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NNS"))),
      //   PropertyWithFilter(List(),
      //     FilterWithModifier(List(Token("by", "IN")),
      //       NamedValue(List(Token("George", "NNP"), Token("Orwell", "NNP")))))))

      val env = new WikidataEnvironment()

      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expectedNodes = List(book.out(P.hasAuthor, author))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q571
          |  { ?1  wdt:P50     ?2 .
          |    ?2  rdfs:label  "George Orwell"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Which/WDT city/NN is/VBZ bigger/JJR than/IN "
                                            + "New/NNP York/NNP City/NNP")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("city", "NN"))),
      //   PropertyWithFilter(List(Token("is", "VBZ")),
      //     FilterWithComparativeModifier(List(Token("bigger", "JJR"), Token("than", "IN")),
      //       NamedValue(List(Token("New", "NNP"), Token("York", "NNP"), Token("City", "NNP")))))))

      val env = new WikidataEnvironment()

      val city = env.newNode()
          .out(P.isA, Q.city)

      val newYorkCity = env.newNode()
          .out(NameLabel, "New York City")

      val otherArea = env.newNode()
          .in(newYorkCity, P.hasArea)

      val area = env.newNode()
          .filter(GreaterThanFilter(otherArea))

      val expectedNodes = List(city.out(P.hasArea, area))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q515
          |  { ?1  wdt:P2046   ?4 .
          |    ?2  wdt:P2046   ?3 ;
          |        rdfs:label  "New York City"@en
          |    FILTER ( ?4 > ?3 )
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Who/WP is/VBZ older/JJR than/IN Obama/NNP")

      // PersonListQuestion(PropertyWithFilter(List(Token("is", "VBZ")),
      //   FilterWithComparativeModifier(List(Token("older", "JJR"), Token("than", "IN")),
      //     NamedValue(List(Token("Obama", "NNP"))))))

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val obama = env.newNode()
          .out(NameLabel, "Obama")

      val otherBirthDate = env.newNode()
          .in(obama, P.hasDateOfBirth)

      val birthDate = env.newNode()
          .filter(LessThanFilter(otherBirthDate))

      val expectedNodes = List(person.out(P.hasDateOfBirth, birthDate))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q5
          |  { ?1  wdt:P569    ?4 .
          |    ?2  wdt:P569    ?3 ;
          |        rdfs:label  "Obama"@en
          |    FILTER ( ?4 < ?3 )
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("which/WDT mountains/NNS are/VBP more/JJR than/IN "
                                            + "1000/CD meters/NNS high/JJ")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("mountains", "NNS"))),
      //   AdjectivePropertyWithFilter(List(Token("are", "VBP"), Token("high", "JJ")),
      //     FilterWithComparativeModifier(List(Token("more", "JJR"), Token("than", "IN")),
      //       NumberWithUnit(List(Token("1000", "CD")), List(Token("meters", "NNS")))))))


      val env = new WikidataEnvironment()

      val mountain = env.newNode()
          .out(P.isA, Q.mountain)

      val minElevation: WikidataNode = (1000.0, U.meter)

      val elevation = env.newNode()
          .filter(GreaterThanFilter(minElevation))

      val expectedNodes = List(mountain.out(P.hasElevation, elevation))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q8502
          |  { ?1  wdt:P2044  ?2
          |    FILTER ( ?2 > "1000.0"^^xsd:integer )
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Which/WDT cities/NNS have/VBP more/JJR than/IN "
                                            + "two/CD million/CD inhabitants/NNS")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("cities", "NNS"))),
      //   PropertyWithFilter(List(Token("have", "VBP")),
      //     FilterWithComparativeModifier(List(Token("more", "JJR"), Token("than", "IN")),
      //       NumberWithUnit(List(Token("two", "CD"), Token("million", "CD")),
      //         List(Token("inhabitants", "NNS")))))))

      val env = new WikidataEnvironment()

      val city = env.newNode()
          .out(P.isA, Q.city)

      val minPopulation: WikidataNode = 2000000.0

      val population = env.newNode()
          .filter(GreaterThanFilter(minPopulation))

      val expectedNodes = List(city.out(P.hasPopulation, population))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q515
          |  { ?1  wdt:P1082  ?2
          |    FILTER ( ?2 > "2000000.0"^^xsd:double )
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      // TODO: adjective "californian": P.isLocatedIn

      val actualNodes = compileListQuestion("In/IN which/WDT californian/JJ cities/NNS "
                                            + "live/VBP more/JJR than/IN "
                                            + "2/CD million/CD people/NNS")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("californian", "NN"),
      //   Token("cities", "NNS"))),
      //   PropertyWithFilter(List(Token("live", "VBP")),
      //     FilterWithComparativeModifier(List(Token("more", "JJR"), Token("than", "IN")),
      //       NumberWithUnit(List(Token("2", "CD"), Token("million", "CD")),
      //         List(Token("people", "NNS")))))))

      val env = new WikidataEnvironment()

      val city = env.newNode()
          .out(P.isA, Q.city)

      val minPopulation: WikidataNode = 2000000.0

      val population = env.newNode()
          .filter(GreaterThanFilter(minPopulation))

      val expectedNodes = List(city.out(P.hasPopulation, population))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q515
          |  { ?1  wdt:P1082  ?2
          |    FILTER ( ?2 > "2000000.0"^^xsd:double )
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Who/WP is/VBD the/DT discoverer/NNS of/IN Pluto/NNP")

      val env = new WikidataEnvironment()

      val pluto = env.newNode()
          .out(NameLabel, "Pluto")

      val discoverer = env.newNode()
          .in(pluto, P.hasDiscovererOrInventor)

      val expectedNodes = List(discoverer)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("2",
        """
          |{ ?1  wdt:P61     ?2 ;
          |      rdfs:label  "Pluto"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Who/WP discovered/VBD the/DT most/JJS planets/NNS")

      // PersonListQuestion(PropertyWithFilter(List(Token("discovered", "VBD")),
      //   PlainFilter(NamedValue(List(Token("the", "DT"), Token("most", "JJS"),
      //     Token("planets", "NNS"))))))

      val env = new WikidataEnvironment()

      val discoverer = env.newNode()
          .out(P.isA, Q.human)

      val planet = env.newNode()
          .out(P.isA, Q.planet)
          .aggregate(Count)
          .aggregate(Max)

      val expectedNodes = List(discoverer
          .in(planet, P.hasDiscovererOrInventor))

      assertEquals(expectedNodes, actualNodes)

      // TODO: implement aggregates in SPARQL compiler
    }
    {
      val actualNodes = compileListQuestion("Who/WP are/VBP the/DT children/NNS "
                                            + "of/IN Clinton/NNP 's/POS spouse/NNS")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"), Token("children", "NNS"))),
      //   RelationshipQuery(NamedQuery(List(Token("spouse", "NNS"))),
      //     NamedQuery(List(Token("Clinton", "NNP"))), Token("'s", "POS")), Token("of", "IN")))

      val env = new WikidataEnvironment()

      val clinton = env.newNode()
          .out(NameLabel, "Clinton")

      val spouse = env.newNode()
          .in(clinton, P.hasSpouse)

      val child = env.newNode()
          .in(spouse, P.hasChild)

      val expectedNodes = List(child)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("3", """
          |{ ?2  wdt:P40     ?3 .
          |  ?1  wdt:P26     ?2 ;
          |      rdfs:label  "Clinton"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Which/WDT books/NNS "
                                            + "were/VBD written/VBN by/IN Jane/NNP Austen/NNP")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NNS"))),
      //   PropertyWithFilter(List(Token("were", "VBD"), Token("written", "VBN")),
      //     FilterWithModifier(List(Token("by", "IN")),
      //       NamedValue(List(Token("Jane", "NNP"), Token("Austen", "NNP")))))))

      val env = new WikidataEnvironment()

      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "Jane Austen")

      val expectedNodes = List(book.out(P.hasAuthor, author))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1", """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q571
          |  { ?1  wdt:P50     ?2 .
          |    ?2  rdfs:label  "Jane Austen"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Which/WDT country/NN was/VBD Obama/NNP born/VBN in/IN")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("country", "NN"))),
      //   InversePropertyWithFilter(List(Token("was", "VBD"), Token("born", "VBN"),
      //     Token("in", "IN")),
      //     PlainFilter(NamedValue(List(Token("Obama", "NNP")))))))

      val env = new WikidataEnvironment()

      val country = env.newNode()
          .out(P.isA, Q.country)

      val obama = env.newNode()
          .out(NameLabel, "Obama")

      val place = env.newNode()
          .in(obama, P.hasPlaceOfBirth)

      val expectedNodes = List(country
          .in(place, P.country))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("1", """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q6256
          |  { ?3  wdt:P17  ?1
          |    { ?2 wdt:P19/(wdt:P131)* ?3
          |      { ?2  rdfs:label  "Obama"@en }
          |    }
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Which/WDT year/NN was/VBD Obama/NNP born/VBN in/IN")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("year", "NN"))),
      //   InversePropertyWithFilter(List(Token("was", "VBD"), Token("born", "VBN"),
      //     Token("in", "IN")),
      //     PlainFilter(NamedValue(List(Token("Obama", "NNP")))))))

      val env = new WikidataEnvironment()

      val year = env.newNode()
          .out(P.isA, Q.year)

      val obama = env.newNode()
          .out(NameLabel, "Obama")

      val date = env.newNode()
          .in(obama, P.hasDateOfBirth)

      val expectedNodes = List(year
          .in(date, YearLabel))

      assertEquals(expectedNodes, actualNodes)

      // TODO: implement YearLabel as binding of year of root variable
    }
    {
      val actualNodes = compileListQuestion("What/WP are/VBP some/DT of/IN Seth/NNP Gabel/NNP 's/POS father-in-law/NN 's/POS movies/NNS")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("movies", "NNS"))),
      //   RelationshipQuery(NamedQuery(List(Token("father-in-law", "NN"))),
      //     NamedQuery(List(Token("Seth", "NNP"), Token("Gabel", "NNP"))),
      //     Token("'s", "POS")),
      //   Token("'s", "POS")))

      val env = new WikidataEnvironment()

      val sethGabel = env.newNode()
          .out(NameLabel, "Seth Gabel")

      val child = env.newNode()
          .out(P.hasSpouse, sethGabel)

      val fatherInLaw = env.newNode()
          .out(P.hasGender, Q.male)
          .out(P.hasChild, child)

      val movie = env.newNode()
          .out(P.isA, Q.movie)

      val expectedNodes = List(movie
          .out(P.hasDirector, fatherInLaw))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head)

      val expectedQuery = parseSparqlQuery("4", """
          |{ ?4 p:P31/(v:P31/(wdt:P279)*) wd:Q11424
          |  { ?4  wdt:P57     ?3 .
          |    ?3  wdt:P21     wd:Q6581097 ;
          |        wdt:P40     ?2 .
          |    ?2  wdt:P26     ?1 .
          |    ?1  rdfs:label  "Seth Gabel"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
  }

  def testSparql() {
    {
      val env = new WikidataEnvironment()

      val pluto = env.newNode()
          .out(NameLabel, "Pluto")

      val nix = env.newNode()
          .out(NameLabel, "Nix")

      val root = env.newNode()
          .out(P.isA, Q.human)
          .and(in(pluto, P.hasDiscovererOrInventor)
               or in(nix, P.hasDiscovererOrInventor))

      val actualQuery = compileSparqlQuery(root)

      val expectedQuery = parseSparqlQuery("3", """
          |{ ?3 p:P31/(v:P31/(wdt:P279)*) wd:Q5
          |    { ?1  wdt:P61     ?3 ;
          |          rdfs:label  "Pluto"@en
          |    }
          |  UNION
          |    { ?2  wdt:P61     ?3 ;
          |          rdfs:label  "Nix"@en
          |    }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val movie = env.newNode()
          .out(NameLabel, "Alien")

      val root = person.in(movie, P.hasCastMember)

      val actualQuery = compileSparqlQuery(root)

      val expectedQuery = parseSparqlQuery("1", """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
          |  { ?2  wdt:P161    ?1 ;
          |        rdfs:label  "Alien"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val env = new WikidataEnvironment()

      val elevation: WikidataNode = (1000.0, U.meter)

      val root = env.newNode()
          .out(P.isA, Q.mountain)
          .out(P.hasElevation, elevation)

      val actualQuery = compileSparqlQuery(root)

      val expectedQuery = parseSparqlQuery("1", """
          |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q8502
          |  { ?1  wdt:P2044   "1000.0"^^<http://www.w3.org/2001/XMLSchema#integer> }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val env = new WikidataEnvironment()

      val number: WikidataNode = Node(NumberLabel(23))

      val other = env.newNode()

      val otherArea = env.newNode()
          .filter(GreaterThanFilter(number))
          .in(other, P.hasArea)

      val area = env.newNode()
          .filter(LessThanFilter(otherArea))

      val root = env.newNode()
          .out(P.hasArea, area)

      val actualQuery = compileSparqlQuery(root)

      val expectedQuery = parseSparqlQuery("4", """
          |{ ?4  wdt:P2046  ?3 .
          |  ?1  wdt:P2046  ?2
          |  FILTER ( ?2 > "23.0"^^xsd:double )
          |  FILTER ( ?3 < ?2 )
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val env = new WikidataEnvironment()

      val number: WikidataNode = Node(NumberLabel(23))

      val root = env.newNode()
          .filter(LessThanFilter(number))

      try {
        val actualQuery = compileSparqlQuery(root)
        fail("should not compile")
      } catch {
        case e: RuntimeException =>
      }
    }
    {
      val env = new WikidataEnvironment()

      val president = env.newNode()
          .in(env.newNode(), P.hasHeadOfState)

      val year: WikidataNode = Year.of(1900)

      val date = env.newNode()
          .filter(LessThanFilter(year))

      val root = president.out(P.hasDateOfBirth, date)

      val actualQuery = compileSparqlQuery(root)

      // TODO: extract year of ?2
      val expectedQuery = parseSparqlQuery("1", """
          |{ ?2  wdt:P35  ?1
          |  { ?1  wdt:P569  ?3
          |    FILTER ( ?3 < 1900 )
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val env = new WikidataEnvironment()

      val city = env.newNode()
          .out(P.isA, Q.city)

      val newYorkCity = env.newNode()
          .out(NameLabel, "New York City")

      val otherArea = env.newNode()
          .in(newYorkCity, P.hasArea)

      val area = env.newNode()
          .filter(GreaterThanFilter(otherArea))

      val root = city.out(P.hasArea, area)

      val actualQuery = compileSparqlQuery(root)

      val expectedQuery = parseSparqlQuery("1", """
          |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q515
          |  { { ?1  wdt:P2046  ?4 .
          |      ?2  wdt:P2046  ?3 .
          |      ?2  rdfs:label  "New York City"@en .
          |      }
          |    FILTER ( ?4 > ?3 )
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val env = new WikidataEnvironment()

      val min: WikidataNode = Node(NumberLabel(23))

      val max: WikidataNode = Node(NumberLabel(42))

      val area = env.newNode()
          .filter(GreaterThanFilter(min)
                  and LessThanFilter(max))

      val root = env.newNode()
          .out(P.hasArea, area)

      val actualQuery = compileSparqlQuery(root)

      val expectedQuery = parseSparqlQuery("2", """
          |{ ?2  wdt:P2046  ?1 .
          |  FILTER ( ?1 > "23.0"^^xsd:double )
          |  FILTER ( ?1 < "42.0"^^xsd:double )
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
  }
}