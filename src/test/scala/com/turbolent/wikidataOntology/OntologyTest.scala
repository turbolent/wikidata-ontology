package com.turbolent.wikidataOntology

import java.time.Year

import com.turbolent.questionCompiler.QuestionCompiler
import com.turbolent.questionCompiler.graph._
import com.turbolent.questionCompiler.sparql.SparqlGraphCompiler
import com.turbolent.questionParser.{BaseParser, ListParser, Token}
import junit.framework.TestCase
import org.apache.jena.query.{Query, QueryFactory}
import org.apache.jena.sparql.algebra.Algebra
import org.hamcrest.CoreMatchers.instanceOf
import org.hamcrest.Matcher
import org.hamcrest.MatcherAssert.assertThat
import org.junit.Assert._


class OntologyTest extends TestCase {

  def compileListQuestion(sentence: String) = {
    val tokens = tokenizeSentence(sentence)
    val result = ListParser.parse(tokens, ListParser.phrase(ListParser.Question))
    assertSuccess(result)
    val question = result.get
    new QuestionCompiler(WikidataOntology, new WikidataEnvironment())
        .compileQuestion(question)
  }

  def tokenizeSentence(taggedSentence: String) =
    taggedSentence.split(' ').map(_.split('/')).map {
      case Array(word, tag, lemma) => Token(word, tag, lemma)
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

  def compileSparqlQuery(node: WikidataNode, env: WikidataEnvironment) =
    new SparqlGraphCompiler(new WikidataSparqlBackend, env)
        .compileQuery(node)

  def testQuestions() {
    {
      val actualNodes = compileListQuestion("who/WP/who acted/VBD/act in/IN/in Alien/NNP/alien")

      // PersonListQuestion(PropertyWithFilter(List(Token("acted", "VBD", "act")),
      //   FilterWithModifier(List(Token("in", "IN", "in")), NamedValue(List(Token("Alien", "NNP", "alien"))))))

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val movie = env.newNode()
          .out(NameLabel, "Alien")

      val expectedNodes = List(person.in(movie, P.hasCastMember))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("who/WP/who starred/VBD/star in/IN/in Inception/NNP/inception")

      // PersonListQuestion(PropertyWithFilter(List(Token("starred", "VBD", "star")),
      //   FilterWithModifier(List(Token("in", "IN", "in")),
      //     NamedValue(List(Token("Inception", "NNP", "inception")))))))

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val movie = env.newNode()
          .out(NameLabel, "Inception")

      val expectedNodes = List(person.in(movie, P.hasCastMember))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Movies/NNP/movie starring/VB/star Winona/NNP/winona Ryder/NNP/ryder")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("Movies", "NNP", "movie"))),
      //   PropertyWithFilter(List(Token("starring", "VB", "star")),
      //     PlainFilter(NamedValue(List(Token("Winona", "NNP", "winona"), Token("Ryder", "NNP", "ryder")))))))

      val env = new WikidataEnvironment()

      val movie = env.newNode().out(P.isA, Q.movie)

      val actress = env.newNode()
          .out(NameLabel, "Winona Ryder")

      val expectedNodes = List(movie.out(P.hasCastMember, actress))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("In/IN/in what/WDT/what movies/NN/movie did/VBD/do "
        + "Jennifer/NNP/jennifer Aniston/NNP/aniston appear/VB/appear")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("movies", "NN", "movie"))),
      //   InversePropertyWithFilter(List(Token("did", "VBD", "do"), Token("appear", "VB", "appear")),
      //     PlainFilter(NamedValue(List(Token("Jennifer", "NNP", "jennifer"), Token("Aniston", "NNP", "aniston")))))))

      val env = new WikidataEnvironment()

      val movie = env.newNode().out(P.isA, Q.movie)

      val actress = env.newNode()
          .out(NameLabel, "Jennifer Aniston")

      val expectedNodes = List(movie.out(P.hasCastMember, actress))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("who/WP/who are/VBP/be the/DT/the actors/NNS/actor "
        + "which/WDT/which starred/VBD/star in/IN/in Inception/NNP/inception")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("the", "DT", "the"),
      //   Token("actors", "NNS", "actor"))),
      //     PropertyWithFilter(List(Token("starred", "VBD", "star")),
      //       FilterWithModifier(List(Token("in", "IN", "in")),
      //         NamedValue(List(Token("Inception", "NNP", "inception")))))))

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.actor)
          .or(out(P.hasOccupation, Q.actor))

      val movie = env.newNode()
          .out(NameLabel, "Inception")

      val expectedNodes = List(person.in(movie, P.hasCastMember))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("who/WP/who directed/VBD/direct Pocahontas/NNP/pocahontas")

      // PersonListQuestion(PropertyWithFilter(List(Token("directed", "VBD", "direct")),
      //   PlainFilter(NamedValue(List(Token("Pocahontas", "NNP", "pocahontas"))))))

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val movie = env.newNode()
          .out(NameLabel, "Pocahontas")

      val expectedNodes = List(person.in(movie, P.hasDirector))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Who/WP/who did/VBD/do Bill/NNP/bill Clinton/NNP/clinton marry/VB/marry")

      // PersonListQuestion(InversePropertyWithFilter(List(Token("did", "VBD", "do"),
      //   Token("marry", "VB", "marry")),
      //   PlainFilter(NamedValue(List(Token("Bill", "NNP", "bill"), Token("Clinton", "NNP", "clinton"))))))

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val bill = env.newNode()
          .out(NameLabel, "Bill Clinton")

      val expectedNodes = List(person.out(P.hasSpouse, bill))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("which/WDT/which mountains/NNS/mountain"
        + " are/VBP/be 1000/CD/1000 meters/NNS/meter high/JJ/high")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("mountains", "NNS", "mountain"))),
      //   AdjectivePropertyWithFilter(List(Token("are", "VBP", "be"), Token("high", "JJ", "high")),
      //     PlainFilter(NumberWithUnit(List(Token("1000", "CD", "1000")), List(Token("meters", "NNS", "meter")))))))

      val env = new WikidataEnvironment()

      val elevation: WikidataNode = (1000.0, U.meter)

      val mountain = env.newNode()
          .out(P.isA, Q.mountain)
          .out(P.hasElevation, elevation)

      val expectedNodes = List(mountain)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q8502
          |  { ?1  wdt:P2044  "1000.0"^^xsd:integer }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("authors/NNS/author which/WDT/which"
        + " died/VBD/die in/IN/in Berlin/NNP/berlin")

      //    ListQuestion(QueryWithProperty(NamedQuery(List(Token("authors", "NNS", "author"))),
      //      PropertyWithFilter(List(Token("died", "VBD", "die")),
      //        FilterWithModifier(List(Token("in", "IN", "in")),
      //          NamedValue(List(Token("Berlin", "NNP", "berlin")))))))

      val env = new WikidataEnvironment()

      val author = env.newNode()
          .in(env.newNode(), P.hasAuthor)

      val place = env.newNode()
          .out(NameLabel, "Berlin")

      val expectedNodes = List(author.out(P.hasPlaceOfDeath, place))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("authors/NNS/author which/WDT/which "
        + "were/VBD/be born/VBD/bear in/IN/in Berlin/NNP/berlin "
        + "and/CC/and died/VBD/die in/IN/in Paris/NNP/paris")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("authors", "NNS", "author"))),
      //   AndProperty(List(PropertyWithFilter(List(Token("were", "VBD", "be"),
      //     Token("born", "VBD", "bear")), FilterWithModifier(List(Token("in", "IN", "in")),
      //     NamedValue(List(Token("Berlin", "NNP", "berlin"))))),
      //     PropertyWithFilter(List(Token("died", "VBD", "die")),
      //       FilterWithModifier(List(Token("in", "IN", "in")),
      //         NamedValue(List(Token("Paris", "NNP", "paris")))))))))

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

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("who/WP/who was/VBD/be born/VBD/bear"
        + " in/IN/in Berlin/NNP/berlin"
        + " or/CC/or died/VBD/die in/IN/in Paris/NNP/paris")

      // PersonListQuestion(OrProperty(List(PropertyWithFilter(List(Token("was", "VBD", "be"),
      //   Token("born", "VBD", "bear")), FilterWithModifier(List(Token("in", "IN", "in")),
      //   NamedValue(List(Token("Berlin", "NNP", "berlin"))))),
      //   PropertyWithFilter(List(Token("died", "VBD", "die")),
      //     FilterWithModifier(List(Token("in", "IN", "in")),
      //       NamedValue(List(Token("Paris", "NNP", "paris"))))))))

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

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Which/WDT/which presidents/NNS/president "
        + "were/VBD/be born/VBN/bear before/IN/before 1900/CD/1900")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("presidents", "NNS", "president"))),
      //   PropertyWithFilter(List(Token("were", "VBD", "be"), Token("born", "VBN", "bear")),
      //     FilterWithModifier(List(Token("before", "IN", "before")), Number(List(Token("1900", "CD", "1900")))))))

      val env = new WikidataEnvironment()

      val president = env.newNode()
          .out(P.holdsPosition, Q.president)

      val year: WikidataNode = Year.of(1900)

      val date = env.newNode()
          .filter(LessThanFilter(year))

      val expectedNodes = List(president.out(P.hasDateOfBirth, date))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P39/(v:P39/(wdt:P279)*) wd:Q30461
          |  { ?1  wdt:P569  ?2
          |    FILTER ( year(?2) < 1900 )
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Give/VB/give me/PRP/me all/DT/all actors/NNS/actor"
        + " born/VBN/bear in/IN/in"
        + " Berlin/NNP/berlin or/CC/or San/NNP/san Francisco/NNP/francisco")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("actors", "NNS", "actor"))),
      //   PropertyWithFilter(List(Token("born", "VBN", "bear")), FilterWithModifier(List(Token("in", "IN", "in")),
      //     OrValue(List(NamedValue(List(Token("Berlin", "NNP", "berlin"))),
      //       NamedValue(List(Token("San", "NNP", "san"), Token("Francisco", "NNP", "francisco")))))))))

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

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Which/WDT/which movies/NNS/movie "
        + "were/VBD/be filmed/VBD/film in/IN/in Germany/NNP/germany "
        + "and/CC/and Denmark/NNP/denmark or/CC/or California/NNP/california")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("movies", "NNS", "movie"))),
      //   PropertyWithFilter(List(Token("were", "VBD", "be"), Token("filmed", "VBD", "film")),
      //     FilterWithModifier(List(Token("in", "IN", "in")),
      //       OrValue(List(AndValue(List(NamedValue(List(Token("Germany", "NNP", "germany"))),
      //         NamedValue(List(Token("Denmark", "NNP", "denmark"))))),
      //         NamedValue(List(Token("California", "NNP", "california")))))))))

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

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Give/VB/give me/PRP/me all/DT/all "
        + "musicians/NNS/musician that/WDT/that "
        + "were/VBD/be born/VBN/bear in/IN/in Vienna/NNP/vienna "
        + "and/CC/and died/VBN/die in/IN/in Berlin/NNP/berlin")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("musicians", "NNS", "musician"))),
      //   AndProperty(List(PropertyWithFilter(List(Token("were", "VBD", "be"), Token("born", "VBN", "bear")),
      //     FilterWithModifier(List(Token("in", "IN", "in")),
      //       NamedValue(List(Token("Vienna", "NNP", "vienna"))))),
      //     PropertyWithFilter(List(Token("died", "VBN", "die")),
      //       FilterWithModifier(List(Token("in", "IN", "in")),
      //         NamedValue(List(Token("Berlin", "NNP", "berlin")))))))))

      val env = new WikidataEnvironment()

      val musician = env.newNode()
          .out(P.isA, Q.musician)
          .or(out(P.hasOccupation, Q.musician))

      val place = env.newNode()
          .out(NameLabel, "Vienna")

      val place2 = env.newNode()
          .out(NameLabel, "Berlin")

      val expectedNodes = List(musician
          .out(P.hasPlaceOfBirth, place)
          .out(P.hasPlaceOfDeath, place2))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Which/WDT/which books/NN/book"
        + " did/VBD/do George/NNP/george Orwell/NNP/orwell write/VB/write")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NN", "book"))),
      //   InversePropertyWithFilter(List(Token("did", "VBD", "do"), Token("write", "VB", "write")),
      //     PlainFilter(NamedValue(List(Token("George", "NNP", "george"), Token("Orwell", "NNP", "orwell")))))))

      val env = new WikidataEnvironment()

      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expectedNodes = List(book.out(P.hasAuthor, author))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("list/VB/list presidents/NNS/president"
        + " of/IN/of Argentina/NNP/argentina")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("presidents", "NNS", "president"))),
      //   NamedQuery(List(Token("Argentina", "NNP", "argentina"))), Token("of", "IN", "of")))

      val env = new WikidataEnvironment()

      val country = env.newNode()
          .out(NameLabel, "Argentina")

      val person = env.newNode()
          .in(country, P.hasHeadOfState)

      val expectedNodes = List(person)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("2",
        """
          |{ ?1  wdt:P35     ?2 ;
          |      rdfs:label  "Argentina"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("List/VB/list albums/NNS/album"
        + " of/IN/of Pink/NNP/pink Floyd/NNP/floyd")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("albums", "NNS", "album"))),
      //   NamedQuery(List(Token("Pink", "NNP", "pink"), Token("Floyd", "NNP", "floyd"))), Token("of", "IN", "of")))

      val env = new WikidataEnvironment()

      val artist = env.newNode()
          .out(NameLabel, "Pink Floyd")

      val album = env.newNode()
          .out(P.isA, Q.album)
          .out(P.hasPerformer, artist)

      val expectedNodes = List(album)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("List/VB/list the/DT/the actors/NNS/actor"
        + " of/IN/of Titanic/NNP/titanic")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
      //   Token("actors", "NNS", "actor"))),
      //   NamedQuery(List(Token("Titanic", "NNP", "titanic"))), Token("of", "IN", "of")))

      val env = new WikidataEnvironment()

      val movie = env.newNode()
          .out(NameLabel, "Titanic")

      val actor = env.newNode()
          .out(P.hasOccupation, Q.actor)
          .in(movie, P.hasCastMember)

      val expectedNodes = List(actor)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Who/WP/who is/VBZ/be the/DT/the director/NN/director"
        + " of/IN/of Big/NN/big Fish/NN/fish")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
      //   Token("director", "NN", "director"))),
      //   NamedQuery(List(Token("Big", "NN", "big"), Token("Fish", "NN", "fish"))), Token("of", "IN", "of")))

      val env = new WikidataEnvironment()

      val movie = env.newNode()
          .out(NameLabel, "Big Fish")

      val director = env.newNode()
          .out(P.isA, Q.filmDirector)
          .in(movie, P.hasDirector)

      val expectedNodes = List(director)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("What/WP/what are/VBP/be the/DT/the members/NNS/member"
        + " of/IN/of Metallica/NNP/metallica")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
      //     Token("members", "NNS", "member"))),
      //   NamedQuery(List(Token("Metallica", "NNP", "metallica"))), Token("of", "IN", "of")))

      val env = new WikidataEnvironment()

      val band = env.newNode()
          .out(NameLabel, "Metallica")

      val member = env.newNode()
          .out(P.isMemberOf, band)

      val expectedNodes = List(member)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("2",
        """
          |{ ?2  wdt:P463    ?1 .
          |  ?1  rdfs:label  "Metallica"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("members/NNS/member of/IN/of Metallica/NNP/metallica")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("members", "NNS", "member"))),
      //   NamedQuery(List(Token("Metallica", "NNP", "metallica"))), Token("of", "IN", "of")))

      val env = new WikidataEnvironment()

      val band = env.newNode()
          .out(NameLabel, "Metallica")

      val member = env.newNode()
          .out(P.isMemberOf, band)

      val expectedNodes = List(member)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("2",
        """
          |{ ?2  wdt:P463    ?1 .
          |  ?1  rdfs:label  "Metallica"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("What/WP/what is/VBZ/be the/DT/the music/NN/music genre/NN/genre"
        + " of/IN/of Gorillaz/NNP/gorillaz")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
      //     Token("music", "NN", "music"), Token("genre", "NN", "genre"))),
      //   NamedQuery(List(Token("Gorillaz", "NNP", "gorillaz"))), Token("of", "IN", "of")))

      val env = new WikidataEnvironment()

      val band = env.newNode()
          .out(NameLabel, "Gorillaz")

      val genre = env.newNode()
          .out(P.isA, Q.musicGenre)
          .in(band, P.hasGenre)

      val expectedNodes = List(genre)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("What/WP/what is/VBZ/be the/DT/the cast/NN/cast of/IN/of Friends/NNS/friend")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"), Token("cast", "NN", "cast"))),
      //   NamedQuery(List(Token("Friends", "NNS", "friend"))), Token("of", "IN", "of")))

      val env = new WikidataEnvironment()

      val show = env.newNode()
          .out(NameLabel, "Friends")

      val member = env.newNode()
          .in(show, P.hasCastMember)

      val expectedNodes = List(member)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("2",
        """
          |{ ?1  wdt:P161    ?2 ;
          |      rdfs:label  "Friends"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("who/WP/who are/VBP/be the/DT/the children/NNS/child"
        + " of/IN/of the/DT/the presidents/NNS/president")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
      //     Token("children", "NNS", "child"))),
      //   NamedQuery(List(Token("the", "DT", "the"), Token("presidents", "NNS", "president"))),
      //     Token("of", "IN", "of")))

      val env = new WikidataEnvironment()

      val president = env.newNode()
          .out(P.holdsPosition, Q.president)

      val child = env.newNode()
          .in(president, P.hasChild)

      val expectedNodes = List(child)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("2",
        """
          |{ ?1  wdt:P40  ?2
          |  { ?1 p:P39/(v:P39/(wdt:P279)*) wd:Q30461 }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("what/WP/what are/VBP/be the/DT/the population/NN/population sizes/NNS/size"
        + " of/IN/of cities/NNS/city located/VBN/locate in/IN/in california/NN/california")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
      //   Token("population", "NN", "population"),
      //   Token("sizes", "NNS", "size"))),
      //   QueryWithProperty(NamedQuery(List(Token("cities", "NNS", "city"))),
      //     PropertyWithFilter(List(Token("located", "VBN", "locate")),
      //       FilterWithModifier(List(Token("in", "IN", "in")),
      //         NamedValue(List(Token("california", "NN", "california")))))),
      //   Token("of", "IN", "of")))

      val env = new WikidataEnvironment()

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

      val expectedNodes = List(population)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("3",
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
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Who/WP/who are/VBP/be the/DT/the children/NNS/child "
        + "of/IN/of the/DT/the children/NNS/child "
        + "of/IN/of Bill/NNP/bill Clinton/NNP/clinton")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
      //   Token("children", "NNS", "child"))),
      //   RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"), Token("children", "NNS", "child"))),
      //     NamedQuery(List(Token("Bill", "NNP", "bill"), Token("Clinton", "NNP", "clinton"))),
      //     Token("of", "IN", "of")),
      //   Token("of", "IN", "of")))

      val env = new WikidataEnvironment()

      val bill = env.newNode()
          .out(NameLabel, "Bill Clinton")

      val child = env.newNode()
          .in(bill, P.hasChild)

      val grandChild = env.newNode()
          .in(child, P.hasChild)

      val expectedNodes = List(grandChild)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Clinton/NNP/clinton 's/POS/'s daughters/NN/daughter")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("daughters", "NN", "daughter"))),
      //     NamedQuery(List(Token("Clinton", "NNP", "clinton"))),
      //   Token("'s", "POS", "'s")))

      val env = new WikidataEnvironment()

      val bill = env.newNode()
          .out(NameLabel, "Clinton")

      val child = env.newNode()
          .out(P.hasGender, Q.female)
          .in(bill, P.hasChild)

      val expectedNodes = List(child)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("What/WP/what are/VBP/be the/DT/the largest/JJS/large cities/NNS/city"
        + " of/IN/of California/NNP/california")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
      //     Token("largest", "JJS", "large"), Token("cities", "NNS", "city"))),
      //   NamedQuery(List(Token("California", "NNP", "california"))),
      //   Token("of", "IN", "of")))

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
      val actualNodes = compileListQuestion("What/WP/what are/VBP/be the/DT/the biggest/JJS/big cities/NNS/city"
        + " of/IN/of California/NNP/california")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
      //   Token("biggest", "JJS", "big"), Token("cities", "NNS", "city"))),
      //   NamedQuery(List(Token("California", "NNP", "california"))),
      //   Token("of", "IN", "of")))

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
      val actualNodes = compileListQuestion("What/WP/what are/VBP/be California/NNP/california 's/POS/'s"
        + " biggest/JJS/big cities/NNS/city")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("biggest", "JJS", "big"),
      //   Token("cities", "NNS", "city"))),
      //   NamedQuery(List(Token("California", "NNP", "california"))),
      //   Token("'s", "POS", "'s")))

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
      val actualNodes = compileListQuestion("Who/WP/who is/VBZ/be"
        + " Bill/NNP/bill Clinton/NNP/clinton 's/POS/'s daughter/NN/daughter")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("daughter", "NN", "daughter"))),
      //   NamedQuery(List(Token("Bill", "NNP", "bill"), Token("Clinton", "NNP", "clinton"))),
      //   Token("'s", "POS", "'s")))

      val env = new WikidataEnvironment()

      val bill = env.newNode()
          .out(NameLabel, "Bill Clinton")

      val daughter = env.newNode()
          .out(P.hasGender, Q.female)
          .in(bill, P.hasChild)

      val expectedNodes = List(daughter)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Who/WP/who is/VBZ/be Bill/NNP/bill Clinton/NNP/clinton 's/POS/'s"
        + " daughter/NN/daughter 's/POS/'s"
        + " husband/NN/husband 's/POS/'s"
        + " daughter/NN/daughter 's/POS/'s"
        + " grandfather/NN/grandfather")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("grandfather", "NN", "grandfather"))),
      //   RelationshipQuery(NamedQuery(List(Token("daughter", "NN", "daughter"))),
      //     RelationshipQuery(NamedQuery(List(Token("husband", "NN", "husband"))),
      //       RelationshipQuery(NamedQuery(List(Token("daughter", "NN", "daughter"))),
      //         NamedQuery(List(Token("Bill", "NNP", "bill"), Token("Clinton", "NNP", "clinton"))),
      //         Token("'s", "POS", "'s")),
      //       Token("'s", "POS", "'s")),
      //     Token("'s", "POS", "'s")),
      //   Token("'s", "POS", "'s")))

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

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Who/WP/who are/VBP/be the/DT/the daughters/NNS/daughter of/IN/of"
        + " the/DT/the wife/NN/wife of/IN/of"
        + " the/DT/the president/NN/president of/IN/of"
        + " the/DT/the United/NNP/united States/NNPS/state")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
      //   Token("daughters", "NNS", "daughter"))),
      //   RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"), Token("wife", "NN", "wife"))),
      //     RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"), Token("president", "NN", "president"))),
      //       NamedQuery(List(Token("the", "DT", "the"),
      //         Token("United", "NNP", "united"), Token("States", "NNPS", "state"))),
      //       Token("of", "IN", "of")),
      //     Token("of", "IN", "of")),
      //   Token("of", "IN", "of")))

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

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Who/WP/who is/VBZ/be the/DT/the son/NN/son of/IN/of"
        + " the/DT/the actor/NN/actor of/IN/of"
        + " \"/``/\" I/PRP/i ,/,/, Robot/NNP/robot \"/''/\"")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"), Token("son", "NN", "son"))),
      //   RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"), Token("actor", "NN", "actor"))),
      //     NamedQuery(List(Token("I", "PRP", "i"), Token(",", ",", ","), Token("Robot", "NNP", "robot"))),
      //     Token("of", "IN", "of")),
      //   Token("of", "IN", "of")))

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

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("children/NNS/child of/IN/of all/DT/all presidents/NNS/president"
        + " of/IN/of the/DT/the US/NNP/us")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("children", "NNS", "child"))),
      //   RelationshipQuery(NamedQuery(List(Token("all", "DT", "all"), Token("presidents", "NNS", "president"))),
      //     NamedQuery(List(Token("the", "DT", "the"), Token("US", "NNP", "us"))),
      //     Token("of", "IN", "of")),
      //   Token("of", "IN", "of")))

      val env = new WikidataEnvironment()

      val country = env.newNode()
          .out(NameLabel, "the US")

      val president = env.newNode()
          .in(country, P.hasHeadOfState)

      val child = env.newNode()
          .in(president, P.hasChild)

      val expectedNodes = List(child)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("List/VB/list movies/NNS/movie directed/VBN/direct"
        + " by/IN/by Quentin/NNP/quentin Tarantino/NNP/tarantino")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("movies", "NNS", "movie"))),
      //   PropertyWithFilter(List(Token("directed", "VBN", "direct")),
      //     FilterWithModifier(List(Token("by", "IN", "by")),
      //       NamedValue(List(Token("Quentin", "NNP", "quentin"), Token("Tarantino", "NNP", "tarantino")))))))

      val env = new WikidataEnvironment()

      val movie = env.newNode()
          .out(P.isA, Q.movie)

      val director = env.newNode()
          .out(NameLabel, "Quentin Tarantino")

      val expectedNodes = List(movie.out(P.hasDirector, director))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Which/WDT/which movies/NN/movie did/VBD/do"
        + " Mel/NNP/mel Gibson/NNP/gibson direct/VB/direct")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("movies", "NN", "movie"))),
      //   InversePropertyWithFilter(List(Token("did", "VBD", "do"), Token("direct", "VB", "direct")),
      //     PlainFilter(NamedValue(List(Token("Mel", "NNP", "mel"), Token("Gibson", "NNP", "gibson")))))))

      val env = new WikidataEnvironment()

      val movie = env.newNode()
          .out(P.isA, Q.movie)

      val director = env.newNode()
          .out(NameLabel, "Mel Gibson")

      val expectedNodes = List(movie.out(P.hasDirector, director))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("actors/NNP/actor")

      // ListQuestion(NamedQuery(List(Token("actors", "NNP", "actor"))))

      val env = new WikidataEnvironment()

      val actor = env.newNode()
          .out(P.isA, Q.actor)
          .or(out(P.hasOccupation, Q.actor))

      val expectedNodes = List(actor)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("what/WDT/what languages/NNS/language are/VBP/be"
        + " spoken/VBN/speak in/IN/in Switzerland/NNP/switzerland")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("languages", "NNS", "language"))),
      //   PropertyWithFilter(List(Token("are", "VBP", "be"), Token("spoken", "VBN", "speak")),
      //     FilterWithModifier(List(Token("in", "IN", "in")),
      //       NamedValue(List(Token("Switzerland", "NNP", "switzerland")))))))

      val env = new WikidataEnvironment()

      val language = env.newNode()
          .out(P.isA, Q.language)

      val country = env.newNode()
          .out(NameLabel, "Switzerland")

      val expectedNodes = List(language
          .in(country, P.hasOfficialLanguage))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Which/WDT/which books/NN/book did/VBD/do"
        + " Orwell/NNP/orwell or/CC/or Shakespeare/NNP/shakespeare write/VB/write")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NN", "book"))),
      //   InversePropertyWithFilter(List(Token("did", "VBD", "do"), Token("write", "VB", "write")),
      //     PlainFilter(OrValue(List(NamedValue(List(Token("Orwell", "NNP", "orwell"))),
      //       NamedValue(List(Token("Shakespeare", "NNP", "shakespeare")))))))))

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

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("which/WDT/which books/NN/book were/VBD/be authored/VBN/author"
        + " by/IN/by George/NNP/george Orwell/NNP/orwell")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NN", "book"))),
      //   PropertyWithFilter(List(Token("were", "VBD", "be"), Token("authored", "VBN", "author")),
      //     FilterWithModifier(List(Token("by", "IN", "by")),
      //       NamedValue(List(Token("George", "NNP", "george"), Token("Orwell", "NNP", "orwell")))))))

      val env = new WikidataEnvironment()

      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expectedNodes = List(book
          .out(P.hasAuthor, author))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("which/WDT/which instrument/NN/instrument"
        + " did/VBD/do John/NNP/john Lennon/NNP/lennon play/VB/play")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("instrument", "NN", "instrument"))),
      //   InversePropertyWithFilter(List(Token("did", "VBD", "do"), Token("play", "VB", "play")),
      //     PlainFilter(NamedValue(List(Token("John", "NNP", "john"), Token("Lennon", "NNP", "lennon")))))))

      val env = new WikidataEnvironment()

      val instrument = env.newNode()
          .out(P.isA, Q.musicalInstrument)

      val musician = env.newNode()
          .out(NameLabel, "John Lennon")

      val expectedNodes = List(instrument
          .in(musician, P.playsInstrument))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Who/WP/who wrote/VBD/write"
        + " \"/``/\" Le/NNP/le Petit/NNP/petit Prince/NNP/prince \"/''/\""
        + " and/CC/and \"/``/\" Vol/NNP/vol de/NNP/de Nuit/NNP/nuit \"/''/\"")

      // PersonListQuestion(PropertyWithFilter(List(Token("wrote", "VBD", "write")),
      //   PlainFilter(AndValue(List(NamedValue(List(Token("Le", "NNP", "le"),
      //     Token("Petit", "NNP", "petit"), Token("Prince", "NNP", "prince"))),
      //     NamedValue(List(Token("Vol", "NNP", "vol"), Token("de", "NNP", "de"), Token("Nuit", "NNP", "nuit"))))))))

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val book = env.newNode()
          .out(NameLabel, "Le Petit Prince")

      val book2 = env.newNode()
          .out(NameLabel, "Vol de Nuit")

      val expectedNodes = List(person
          .in(book, P.hasAuthor)
          .in(book2, P.hasAuthor))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("What/WP/what did/VBD/do"
        + " George/NNP/george Orwell/NNP/orwell write/VB/write")

      // ThingListQuestion(InversePropertyWithFilter(List(Token("did", "VBD", "do"),
      //   Token("write", "VB", "write")),
      //   PlainFilter(NamedValue(List(Token("George", "NNP", "george"), Token("Orwell", "NNP", "orwell"))))))

      val env = new WikidataEnvironment()

      val thing = env.newNode()

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expectedNodes = List(thing
          .out(P.hasAuthor, author))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1  wdt:P50     ?2 .
          |  ?2  rdfs:label  "George Orwell"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("What/WP/what was/VBD/be authored/VBN/author"
        + " by/IN/by George/NNP/george Orwell/NNP/orwell")

      // ThingListQuestion(PropertyWithFilter(List(Token("was", "VBD", "be"), Token("authored", "VBN", "author")),
      //   FilterWithModifier(List(Token("by", "IN", "by")),
      //     NamedValue(List(Token("George", "NNP", "george"), Token("Orwell", "NNP", "orwell"))))))

      val env = new WikidataEnvironment()

      val thing = env.newNode()

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expectedNodes = List(thing
          .out(P.hasAuthor, author))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1  wdt:P50     ?2 .
          |  ?2  rdfs:label  "George Orwell"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("What/WP/what books/NNP/book were/VBD/be authored/VBN/author"
        + " by/IN/by George/NNP/george Orwell/NNP/orwell")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NNP", "book"))),
      //   PropertyWithFilter(List(Token("were", "VBD", "be"), Token("authored", "VBN", "author")),
      //     FilterWithModifier(List(Token("by", "IN", "by")),
      //       NamedValue(List(Token("George", "NNP", "george"), Token("Orwell", "NNP", "orwell")))))))

      val env = new WikidataEnvironment()

      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expectedNodes = List(book
          .out(P.hasAuthor, author))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("authors/NNS/author who/WP/who died/VBD/die")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("authors", "NNS", "author"))),
      //   NamedProperty(List(Token("died", "VBD", "die")))))

      val env = new WikidataEnvironment()

      val author = env.newNode()
          .in(env.newNode(), P.hasAuthor)

      val place = env.newNode()

      val expectedNodes = List(author
          .and(out(P.hasDateOfDeath, place)
               or out(P.hasPlaceOfDeath, place)))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("authors/NNS/author which/WDT/which died/VBD/die"
        + " in/IN/in Berlin/NNP/berlin")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("authors", "NNS", "author"))),
      //   PropertyWithFilter(List(Token("died", "VBD", "die")),
      //     FilterWithModifier(List(Token("in", "IN", "in")),
      //       NamedValue(List(Token("Berlin", "NNP", "berlin")))))))

      val env = new WikidataEnvironment()

      val author = env.newNode()
          .in(env.newNode(), P.hasAuthor)

      val place = env.newNode()
          .out(NameLabel, "Berlin")

      val expectedNodes = List(author
          .out(P.hasPlaceOfDeath, place))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("What/WDT/what actor/NN/actor married/VBD/marry"
        + " John/NNP/john F./NNP/f. Kennedy/NNP/kennedy 's/POS/'s sister/NN/sister")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("actor", "NN", "actor"))),
      //   PropertyWithFilter(List(Token("married", "VBD", "marry")),
      //     PlainFilter(ValueRelationship(NamedValue(List(Token("sister", "NN", "sister"))),
      //       NamedValue(List(Token("John", "NNP", "john"), Token("F.", "NNP", "f."),
      //         Token("Kennedy", "NNP", "kennedy"))))))))

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

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Who/WP/who did/VBD/do Bill/NNP/bill Clinton/NNP/clinton 's/POS/'s"
        + " daughter/NN/daughter marry/VB/marry")

      // PersonListQuestion(InversePropertyWithFilter(List(Token("did", "VBD", "do"), Token("marry", "VB", "marry")),
      //   PlainFilter(ValueRelationship(NamedValue(List(Token("daughter", "NN", "daughter"))),
      //     NamedValue(List(Token("Bill", "NNP", "bill"), Token("Clinton", "NNP", "clinton")))))))

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

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Clinton/NNP/clinton 's/POS/'s children/NNS/child"
        + " and/CC/and grandchildren/NNS/grandchild")

      // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("children", "NNS", "child"))),
      //   NamedQuery(List(Token("grandchildren", "NNS", "grandchild"))))),
      //   NamedQuery(List(Token("Clinton", "NNP", "clinton"))),
      //   Token("'s", "POS", "'s")))

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
        val actualQuery = compileSparqlQuery(expectedNodes.head, env)

        val expectedQuery = parseSparqlQuery("2",
          """
            |{ ?1  wdt:P40     ?2 ;
            |      rdfs:label  "Clinton"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(1), env)

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
      val actualNodes = compileListQuestion("What/WP/what are/VBP/be the/DT/the population/NN/population"
        + " of/IN/of China/NNP/china and/CC/and the/DT/the USA/NNP/usa")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
      //   Token("population", "NN", "population"))),
      //   AndQuery(List(NamedQuery(List(Token("China", "NNP", "china"))),
      //     NamedQuery(List(Token("the", "DT", "the"), Token("USA", "NNP", "usa"))))),
      //   Token("of", "IN", "of")))

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
        val actualQuery = compileSparqlQuery(expectedNodes.head, env)

        val expectedQuery = parseSparqlQuery("3",
          """
            |{ ?1  wdt:P1082   ?3 ;
            |      rdfs:label  "China"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(1), env)

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
      val actualNodes = compileListQuestion("the/DT/the population/NNP/population"
        + " of/IN/of Japan/NNP/japan and/CC/and China/NNP/china")

      //    ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"),
      //      Token("population", "NNP", "population"))),
      //      AndQuery(List(NamedQuery(List(Token("Japan", "NNP", "japan"))),
      //        NamedQuery(List(Token("China", "NNP", "china"))))),
      //      Token("of", "IN", "of")))

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
        val actualQuery = compileSparqlQuery(expectedNodes.head, env)

        val expectedQuery = parseSparqlQuery("3",
          """
            |{ ?1  wdt:P1082   ?3 ;
            |      rdfs:label  "Japan"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(1), env)

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
      val actualNodes = compileListQuestion("the/DT/the population/NNP/population and/CC/and area/NNP/area"
        + " of/IN/of Japan/NNP/japan and/CC/and China/NNP/china")

      // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("the", "DT", "the"),
      //     Token("population", "NNP", "population"))),
      //   NamedQuery(List(Token("area", "NNP", "area"))))),
      //   AndQuery(List(NamedQuery(List(Token("Japan", "NNP", "japan"))),
      //     NamedQuery(List(Token("China", "NNP", "china"))))),
      //   Token("of", "IN", "of")))

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
        val actualQuery = compileSparqlQuery(expectedNodes.head, env)

        val expectedQuery = parseSparqlQuery("3",
          """
            |{ ?1  wdt:P1082   ?3 ;
            |      rdfs:label  "Japan"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(1), env)

        val expectedQuery = parseSparqlQuery("4",
          """
            |{ ?2  wdt:P1082   ?4 ;
            |      rdfs:label  "China"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(2), env)

        val expectedQuery = parseSparqlQuery("5",
          """
            |{ ?1  wdt:P2046   ?5 ;
            |      rdfs:label  "Japan"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(3), env)

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
      val actualNodes = compileListQuestion("the/DT/the population/NN/population ,/,/,"
        + " land/NN/land area/NN/area and/CC/and capitals/NNP/capital"
        + " of/IN/of Japan/NNP/japan ,/,/, India/NNP/india and/CC/and China/NNP/china")

      // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("the", "DT", "the"),
      //     Token("population", "NN", "population"))),
      //   NamedQuery(List(Token("land", "NN", "land"), Token("area", "NN", "area"))),
      //   NamedQuery(List(Token("capitals", "NNP", "capital"))))),
      //   AndQuery(List(NamedQuery(List(Token("Japan", "NNP", "japan"))),
      //     NamedQuery(List(Token("India", "NNP", "india"))),
      //     NamedQuery(List(Token("China", "NNP", "china"))))),
      //   Token("of", "IN", "of")))

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
        val actualQuery = compileSparqlQuery(expectedNodes.head, env)

        val expectedQuery = parseSparqlQuery("4",
          """
            |{ ?1  wdt:P1082   ?4 ;
            |      rdfs:label  "Japan"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(1), env)

        val expectedQuery = parseSparqlQuery("5",
          """
            |{ ?2  wdt:P1082   ?5 ;
            |      rdfs:label  "India"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(2), env)

        val expectedQuery = parseSparqlQuery("6",
          """
            |{ ?3  wdt:P1082   ?6 ;
            |      rdfs:label  "China"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(3), env)

        val expectedQuery = parseSparqlQuery("7",
          """
            |{ ?1  wdt:P2046   ?7 ;
            |      rdfs:label  "Japan"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(4), env)

        val expectedQuery = parseSparqlQuery("8",
          """
            |{ ?2  wdt:P2046   ?8 ;
            |      rdfs:label  "India"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(5), env)

        val expectedQuery = parseSparqlQuery("9",
          """
            |{ ?3  wdt:P2046   ?9 ;
            |      rdfs:label  "China"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(6), env)

        val expectedQuery = parseSparqlQuery("10",
          """
            |{ ?1  wdt:P36     ?10 ;
            |      rdfs:label  "Japan"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(7), env)

        val expectedQuery = parseSparqlQuery("11",
          """
            |{ ?2  wdt:P36     ?11 ;
            |      rdfs:label  "India"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(8), env)

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
      val actualNodes = compileListQuestion("Japan/NNP/japan and/CC/and China/NNP/china 's/POS/'s"
        + " population/NNP/population and/CC/and area/NNP/area")

      // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("population", "NNP", "population"))),
      //   NamedQuery(List(Token("area", "NNP", "area"))))),
      //   AndQuery(List(NamedQuery(List(Token("Japan", "NNP", "japan"))),
      //     NamedQuery(List(Token("China", "NNP", "china"))))),
      //   Token("'s", "POS", "'s")))

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
        val actualQuery = compileSparqlQuery(expectedNodes.head, env)

        val expectedQuery = parseSparqlQuery("3",
          """
            |{ ?1  wdt:P1082   ?3 ;
            |      rdfs:label  "Japan"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(1), env)

        val expectedQuery = parseSparqlQuery("4",
          """
            |{ ?2  wdt:P1082   ?4 ;
            |      rdfs:label  "China"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(2), env)

        val expectedQuery = parseSparqlQuery("5",
          """
            |{ ?1  wdt:P2046   ?5 ;
            |      rdfs:label  "Japan"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(3), env)

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
      val actualNodes = compileListQuestion("children/NNS/child and/CC/and grandchildren/NNS/grandchild"
        + " of/IN/of Clinton/NNP/clinton")

      // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("children", "NNS", "child"))),
      //   NamedQuery(List(Token("grand", "JJ", "grand"), Token("children", "NNS", "child"))))),
      //   NamedQuery(List(Token("Clinton", "NNP", "clinton"))),
      //   Token("of", "IN", "of")))

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
        val actualQuery = compileSparqlQuery(expectedNodes.head, env)

        val expectedQuery = parseSparqlQuery("2",
          """
            |{ ?1  wdt:P40     ?2 ;
            |      rdfs:label  "Clinton"@en
            |}
          """.stripMargin)

        assertEquivalent(expectedQuery, actualQuery)
      }
      {
        val actualQuery = compileSparqlQuery(expectedNodes(1), env)

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
      val actualNodes = compileListQuestion("who/WP/who wrote/VBD/write books/NNS/book")

      // PersonListQuestion(PropertyWithFilter(List(Token("wrote", "VBD", "write")),
      //     PlainFilter(NamedValue(List(Token("books", "NNS", "book"))))))

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val book = env.newNode()
          .out(P.isA, Q.book)

      val expectedNodes = List(person.in(book, P.hasAuthor))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("presidents/NNS/president that/WDT/that have/VBP/have children/NNS/child")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("presidents", "NNS", "president"))),
      //   PropertyWithFilter(List(Token("have", "VBP", "have")),
      //     PlainFilter(NamedValue(List(Token("children", "NNS", "child")))))))

      val env = new WikidataEnvironment()

      val president = env.newNode()
          .out(P.holdsPosition, Q.president)

      val child = env.newNode()
          .in(env.newNode(), P.hasChild)

      val expectedNodes = List(president.out(P.hasChild, child))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P39/(v:P39/(wdt:P279)*) wd:Q30461
          |  { ?1  wdt:P40  ?2 .
          |    ?3  wdt:P40  ?2
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("what/WP/what are/VBP/be the/DT/the largest/JJS/large cities/NNS/city"
        + " in/IN/in europe/NN/europe")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("the", "DT", "the"),
      //     Token("largest", "JJS", "large"), Token("cities", "NNS", "city"))),
      //   PropertyWithFilter(List(),
      //     FilterWithModifier(List(Token("in", "IN", "in")),
      //       NamedValue(List(Token("europe", "NN", "europe")))))))

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
      val actualNodes = compileListQuestion("List/VB/list books/NNS/book by/IN/by George/NNP/george Orwell/NNP/orwell")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NNS", "book"))),
      //   PropertyWithFilter(List(),
      //     FilterWithModifier(List(Token("by", "IN", "by")),
      //       NamedValue(List(Token("George", "NNP", "george"), Token("Orwell", "NNP", "orwell")))))))

      val env = new WikidataEnvironment()

      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expectedNodes = List(book.out(P.hasAuthor, author))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Which/WDT/which city/NN/city is/VBZ/be bigger/JJR/big"
        + " than/IN/than New/NNP/new York/NNP/york City/NNP/city")

      //    ListQuestion(QueryWithProperty(NamedQuery(List(Token("city", "NN", "city"))),
      //      PropertyWithFilter(List(Token("is", "VBZ", "be")),
      //        FilterWithComparativeModifier(List(Token("bigger", "JJR", "big"), Token("than", "IN", "than")),
      //          NamedValue(List(Token("New", "NNP", "new"), Token("York", "NNP", "york"),
      //            Token("City", "NNP", "city")))))))

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

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("who/WP/who is/VBZ/be older/JJR/old than/IN/than Obama/NNP/obama")

      // PersonListQuestion(PropertyWithFilter(List(Token("is", "VBZ", "be")),
      //   FilterWithComparativeModifier(List(Token("older", "JJR", "old"), Token("than", "IN", "than")),
      //     NamedValue(List(Token("Obama", "NNP", "obama"))))))

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

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("which/WDT/which mountains/NNS/mountain are/VBP/be "
        + "more/JJR/more than/IN/than 1000/CD/1000 meters/NNS/meter high/JJ/high")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("mountains", "NNS", "mountain"))),
      //   AdjectivePropertyWithFilter(List(Token("are", "VBP", "be"), Token("high", "JJ", "high")),
      //     FilterWithComparativeModifier(List(Token("more", "JJR", "more"), Token("than", "IN", "than")),
      //       NumberWithUnit(List(Token("1000", "CD", "1000")), List(Token("meters", "NNS", "meter")))))))

      val env = new WikidataEnvironment()

      val mountain = env.newNode()
          .out(P.isA, Q.mountain)

      val minElevation: WikidataNode = (1000.0, U.meter)

      val elevation = env.newNode()
          .filter(GreaterThanFilter(minElevation))

      val expectedNodes = List(mountain.out(P.hasElevation, elevation))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("Which/WDT/which cities/NNS/city have/VBP/have more/JJR/more than/IN/than "
        + "two/CD/two million/CD/million inhabitants/NNS/inhabitant")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("cities", "NNS", "city"))),
      //   PropertyWithFilter(List(Token("have", "VBP", "have")),
      //     FilterWithComparativeModifier(List(Token("more", "JJR", "more"), Token("than", "IN", "than")),
      //       NumberWithUnit(List(Token("two", "CD", "two"), Token("million", "CD", "million")),
      //         List(Token("inhabitants", "NNS", "inhabitant")))))))

      val env = new WikidataEnvironment()

      val city = env.newNode()
          .out(P.isA, Q.city)

      val minPopulation: WikidataNode = 2000000.0

      val population = env.newNode()
          .filter(GreaterThanFilter(minPopulation))

      val expectedNodes = List(city.out(P.hasPopulation, population))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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

      val actualNodes = compileListQuestion("In/IN/in which/WDT/which californian/JJ/californian cities/NNS/city "
        + "live/VBP/live more/JJR/more than/IN/than 2/CD/2 million/CD/million people/NNS/people")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("californian", "JJ", "californian"),
      //   Token("cities", "NNS", "city"))),
      //   PropertyWithFilter(List(Token("live", "VBP", "live")),
      //     FilterWithComparativeModifier(List(Token("more", "JJR", "more"), Token("than", "IN", "than")),
      //       NumberWithUnit(List(Token("2", "CD", "2"), Token("million", "CD", "million")),
      //         List(Token("people", "NNS", "people")))))))

      val env = new WikidataEnvironment()

      val city = env.newNode()
          .out(P.isA, Q.city)

      val minPopulation: WikidataNode = 2000000.0

      val population = env.newNode()
          .filter(GreaterThanFilter(minPopulation))

      val expectedNodes = List(city.out(P.hasPopulation, population))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

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
      val actualNodes = compileListQuestion("who/WP/who is/VBD/be the/DT/the discoverer/NNS/discoverer"
        + " of/IN/of Pluto/NNP/pluto")

      val env = new WikidataEnvironment()

      val pluto = env.newNode()
          .out(NameLabel, "Pluto")

      val discoverer = env.newNode()
          .in(pluto, P.hasDiscovererOrInventor)

      val expectedNodes = List(discoverer)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("2",
        """
          |{ ?1  wdt:P61     ?2 ;
          |      rdfs:label  "Pluto"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("who/WP/who discovered/VBD/discover"
        + " the/DT/the most/JJS/most planets/NNS/planet")

      // PersonListQuestion(PropertyWithFilter(List(Token("discovered", "VBD", "discover")),
      //   PlainFilter(NamedValue(List(Token("the", "DT", "the"), Token("most", "JJS", "most"),
      //     Token("planets", "NNS", "planet"))))))

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
      val actualNodes = compileListQuestion("who/WP/who are/VBP/be the/DT/the children/NNS/child "
                                            + "of/IN/of Clinton/NNP/clinton 's/POS/'s spouse/NNS/spouse")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT", "the"), Token("children", "NNS", "child"))),
      //   RelationshipQuery(NamedQuery(List(Token("spouse", "NNS", "spouse"))),
      //     NamedQuery(List(Token("Clinton", "NNP", "clinton"))), Token("'s", "POS", "'s")), Token("of", "IN", "of")))

      val env = new WikidataEnvironment()

      val clinton = env.newNode()
          .out(NameLabel, "Clinton")

      val spouse = env.newNode()
          .in(clinton, P.hasSpouse)

      val child = env.newNode()
          .in(spouse, P.hasChild)

      val expectedNodes = List(child)

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("3",
        """
          |{ ?2  wdt:P40     ?3 .
          |  ?1  wdt:P26     ?2 ;
          |      rdfs:label  "Clinton"@en
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Which/WDT/which books/NNS/book "
        + "were/VBD/be written/VBN/write by/IN/by Jane/NNP/jane Austen/NNP/austen")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NNS", "book"))),
      //   PropertyWithFilter(List(Token("were", "VBD", "be"), Token("written", "VBN", "write")),
      //     FilterWithModifier(List(Token("by", "IN", "by")),
      //       NamedValue(List(Token("Jane", "NNP", "jane"), Token("Austen", "NNP", "austen")))))))

      val env = new WikidataEnvironment()

      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "Jane Austen")

      val expectedNodes = List(book.out(P.hasAuthor, author))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q571
          |  { ?1  wdt:P50     ?2 .
          |    ?2  rdfs:label  "Jane Austen"@en
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("Which/WDT/which country/NN/country"
        + " was/VBD/be Obama/NNP/obama born/VBN/bear in/IN/in")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("country", "NN", "country"))),
      //   InversePropertyWithFilter(List(Token("was", "VBD", "be"), Token("born", "VBN", "bear"),
      //     Token("in", "IN", "in")),
      //     PlainFilter(NamedValue(List(Token("Obama", "NNP", "obama")))))))

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

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("1",
        """
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
      val actualNodes = compileListQuestion("Which/WDT/which year/NN/year"
        + " was/VBD/be Obama/NNP/obama born/VBN/bear in/IN/in")

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("year", "NN", "year"))),
      //   InversePropertyWithFilter(List(Token("was", "VBD", "be"), Token("born", "VBN", "bear"),
      //     Token("in", "IN", "in")),
      //     PlainFilter(NamedValue(List(Token("Obama", "NNP", "obama")))))))

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
      val actualNodes = compileListQuestion("What/WP/what are/VBP/be some/DT/some of/IN/of"
        + " Seth/NNP/seth Gabel/NNP/gabel 's/POS/'s"
        + " father-in-law/NN/father-in-law 's/POS/'s"
        + " movies/NNS/movie")

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("movies", "NNS", "movie"))),
      //   RelationshipQuery(NamedQuery(List(Token("father-in-law", "NN", "father-in-law"))),
      //     NamedQuery(List(Token("Seth", "NNP", "seth"), Token("Gabel", "NNP", "gabel"))),
      //     Token("'s", "POS", "'s")),
      //   Token("'s", "POS", "'s")))

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

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("4",
        """
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
    {
      val actualNodes = compileListQuestion("who/WP/who died/VBD/die in/IN/in Paris/NNP/paris")

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val paris = env.newNode()
          .out(NameLabel, "Paris")

      val expectedNodes = List(person
          .out(P.hasPlaceOfDeath, paris))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("1",
        """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
          |  { ?1 wdt:P20/(wdt:P131)* ?2
          |    { ?2  rdfs:label  "Paris"@en }
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("who/WP/who died/VBD/die before/IN/before 1900/CD/1900")
      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val year: WikidataNode = Year.of(1900)

      val date = env.newNode()
          .filter(LessThanFilter(year))

      val expectedNodes = List(person
          .out(P.hasDateOfDeath, date))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("1", """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
          |  { ?1  wdt:P570  ?2
          |    FILTER ( year(?2) < 1900 )
          |  }
          |}
        """.stripMargin)

      assertEquivalent(expectedQuery, actualQuery)
    }
    {
      val actualNodes = compileListQuestion("who/WP/who died/VBD/die in/IN/in 1900/CD/1900")
      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val year: WikidataNode = Year.of(1900)

      val expectedNodes = List(person
          .out(P.hasDateOfDeath, year))

      assertEquals(expectedNodes, actualNodes)

      val actualQuery = compileSparqlQuery(expectedNodes.head, env)

      val expectedQuery = parseSparqlQuery("1", """
          |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
          |  { ?1  wdt:P570  ?2
          |    FILTER ( year(?2) = 1900 )
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

      val actualQuery = compileSparqlQuery(root, env)

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

      val actualQuery = compileSparqlQuery(root, env)

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

      val actualQuery = compileSparqlQuery(root, env)

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

      val actualQuery = compileSparqlQuery(root, env)

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
        val actualQuery = compileSparqlQuery(root, env)
        fail("should not compile")
      } catch {
        case e: RuntimeException =>
      }
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

      val actualQuery = compileSparqlQuery(root, env)

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

      val actualQuery = compileSparqlQuery(root, env)

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