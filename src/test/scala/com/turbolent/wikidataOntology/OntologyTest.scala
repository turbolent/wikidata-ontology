package com.turbolent.wikidataOntology

import java.nio.file.Paths
import java.time.Year

import com.turbolent.lemmatizer.Lemmatizer
import com.turbolent.questionCompiler.QuestionCompiler
import com.turbolent.questionCompiler.graph.{LessThanFilter, GreaterThanFilter, Max, Count}
import com.turbolent.questionParser.{ListParser, BaseParser, Token, ast}
import junit.framework.TestCase
import org.hamcrest.CoreMatchers.instanceOf
import org.hamcrest.Matcher
import org.hamcrest.MatcherAssert.assertThat
import org.junit.Assert.assertEquals


class OntologyTest extends TestCase {

  implicit lazy val lemmatizer =
    Lemmatizer.loadFrom(Paths.get("lemmatizer-model"))

  def parseListQuestion(tokens: Seq[Token]) =
    ListParser.parse(tokens, ListParser.phrase(ListParser.Question))

  def tokenize(taggedSentence: String) =
    taggedSentence.split(' ').toSeq map { taggedWord =>
      val Array(word, tag) = taggedWord.split('/')
      Token(word, tag)
    }

  def assertSuccess(x: Any) = {
    val matcher: Matcher[Any] = instanceOf(classOf[BaseParser#Success[Any]])
    assertThat(x, matcher)
  }

  def compile(question: ast.Question) =
    new QuestionCompiler(WikidataOntology, new WikidataEnvironment())
        .compileQuestion(question)

  def test() {
    {
      val tokens = tokenize("who/WP acted/VBD in/IN Alien/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // PersonListQuestion(PropertyWithFilter(List(Token("acted", "VBD")),
      //   FilterWithModifier(List(Token("in", "IN")), NamedValue(List(Token("Alien", "NNP"))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val movie = env.newNode()
          .out(NameLabel, "Alien")

      val expected = List(person.in(movie, P.hasCastMember))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("who/WP starred/VBD in/IN Inception/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // PersonListQuestion(PropertyWithFilter(List(Token("starred", "VBD")),
      //   FilterWithModifier(List(Token("in", "IN")),
      //     NamedValue(List(Token("Inception", "NNP"))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val movie = env.newNode()
          .out(NameLabel, "Inception")

      val expected = List(person.in(movie, P.hasCastMember))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Movies/NNP starring/VB Winona/NNP Ryder/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("Movies", "NNP"))),
      //   PropertyWithFilter(List(Token("starring", "VB")),
      //     PlainFilter(NamedValue(List(Token("Winona", "NNP"), Token("Ryder", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val movie = env.newNode().out(P.isA, Q.movie)

      val actress = env.newNode()
          .out(NameLabel, "Winona Ryder")

      val expected = List(movie.out(P.hasCastMember, actress))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("In/IN what/WDT movies/NN did/VBD Jennifer/NNP Aniston/NNP appear/VB")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("movies", "NN"))),
      //   InversePropertyWithFilter(List(Token("did", "VBD"), Token("appear", "VB")),
      //     PlainFilter(NamedValue(List(Token("Jennifer", "NNP"), Token("Aniston", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val movie = env.newNode().out(P.isA, Q.movie)

      val actress = env.newNode()
          .out(NameLabel, "Jennifer Aniston")

      val expected = List(movie.out(P.hasCastMember, actress))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("who/WP are/VBP the/DT actors/NNS which/WDT starred/VBD in/IN Inception/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("the", "DT"),
      //   Token("actors", "NNS"))),
      //   PropertyWithFilter(List(Token("starred", "VBD")),
      //     FilterWithModifier(List(Token("in", "IN")),
      //       NamedValue(List(Token("Inception", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.actor)

      val movie = env.newNode()
          .out(NameLabel, "Inception")

      val expected = List(person.in(movie, P.hasCastMember))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("who/WP directed/VBD Pocahontas/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // PersonListQuestion(PropertyWithFilter(List(Token("directed", "VBD")),
      //   PlainFilter(NamedValue(List(Token("Pocahontas", "NNP"))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val movie = env.newNode()
          .out(NameLabel, "Pocahontas")

      val expected = List(person.in(movie, P.hasDirector))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Who/WP did/VBD Bill/NNP Clinton/NNP marry/VB")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // PersonListQuestion(InversePropertyWithFilter(List(Token("did", "VBD"),
      //   Token("marry", "VB")),
      //   PlainFilter(NamedValue(List(Token("Bill", "NNP"), Token("Clinton", "NNP"))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val bill = env.newNode()
          .out(NameLabel, "Bill Clinton")

      val expected = List(person.out(P.hasSpouse, bill))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("which/WDT mountains/NNS are/VBP 1000/CD meters/NNS high/JJ")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("mountains", "NNS"))),
      //   AdjectivePropertyWithFilter(List(Token("are", "VBP"), Token("high", "JJ")),
      //     PlainFilter(NumberWithUnit(List(Token("1000", "CD")), List(Token("meters", "NNS")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val elevation: WikidataNode = (1000.0, U.meter)

      val mountain = env.newNode()
          .out(P.isA, Q.mountain)
          .out(P.hasElevation, elevation)

      val expected = List(mountain)

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("authors/NNS which/WDT died/VBD in/IN Berlin/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("authors", "NNS"))),
      //   PropertyWithFilter(List(Token("died", "VBD")),
      //     FilterWithModifier(List(Token("in", "IN")),
      //       NamedValue(List(Token("Berlin", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val author = env.newNode()
          .in(env.newNode(), P.hasAuthor)

      val place = env.newNode()
          .out(NameLabel, "Berlin")

      val expected = List(author.out(P.hasPlaceOfDeath, place))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("authors/NNS which/WDT were/VBD born/VBD in/IN Berlin/NNP "
                 + "and/CC died/VBD in/IN Paris/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("authors", "NNS"))),
      //   AndProperty(List(PropertyWithFilter(List(Token("were", "VBD"),
      //     Token("born", "VBD")),FilterWithModifier(List(Token("in", "IN")),
      //     NamedValue(List(Token("Berlin", "NNP"))))),
      //     PropertyWithFilter(List(Token("died", "VBD")),
      //       FilterWithModifier(List(Token("in", "IN")),
      //         NamedValue(List(Token("Paris", "NNP")))))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val author = env.newNode()
          .in(env.newNode(), P.hasAuthor)

      val place = env.newNode()
          .out(NameLabel, "Berlin")

      val place2 = env.newNode()
          .out(NameLabel, "Paris")

      val expected = List(author
          .out(P.hasPlaceOfBirth, place)
          .out(P.hasPlaceOfDeath, place2))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("who/WP was/VBD born/VBD in/IN Berlin/NNP or/CC died/VBD in/IN Paris/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // PersonListQuestion(OrProperty(List(PropertyWithFilter(List(Token("was", "VBD"),
      //   Token("born", "VBD")), FilterWithModifier(List(Token("in", "IN")),
      //   NamedValue(List(Token("Berlin", "NNP"))))),
      //   PropertyWithFilter(List(Token("died", "VBD")),
      //     FilterWithModifier(List(Token("in", "IN")),
      //       NamedValue(List(Token("Paris", "NNP"))))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val author = env.newNode()
          .out(P.isA, Q.human)

      val place = env.newNode()
          .out(NameLabel, "Berlin")

      val place2 = env.newNode()
          .out(NameLabel, "Paris")

      val expected = List(author
          .and(out(P.hasPlaceOfBirth, place)
               or out(P.hasPlaceOfDeath, place2)))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Which/WDT presidents/NNS were/VBD born/VBN before/IN 1900/CD")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("presidents", "NNS"))),
      //   PropertyWithFilter(List(Token("were", "VBD"), Token("born", "VBN")),
      //     FilterWithModifier(List(Token("before", "IN")), Number(List(Token("1900", "CD")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val president = env.newNode()
          .out(P.isA, Q.president)

      val year: WikidataNode = Year.of(1900)

      val date = env.newNode()
          .filter(LessThanFilter(year))

      val expected = List(president.out(P.hasDateOfBirth, date))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Give/VB me/PRP all/DT actors/NNS born/VBN in/IN "
                            + "Berlin/NNP or/CC San/NNP Francisco/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("actors", "NNS"))),
      //   PropertyWithFilter(List(Token("born", "VBN")), FilterWithModifier(List(Token("in", "IN")),
      //     OrValue(List(NamedValue(List(Token("Berlin", "NNP"))),
      //       NamedValue(List(Token("San", "NNP"), Token("Francisco", "NNP")))))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val actor = env.newNode()
          .out(P.isA, Q.actor)

      val place = env.newNode()
          .out(NameLabel, "Berlin")

      val place2 = env.newNode()
          .out(NameLabel, "San Francisco")

      val expected = List(actor
          .and(out(P.hasPlaceOfBirth, place)
               or out(P.hasPlaceOfBirth, place2)))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Which/WDT movies/NNS were/VBD filmed/VBD in/IN Germany/NNP" +
                            " and/CC Denmark/NNP or/CC California/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("movies", "NNS"))),
      //   PropertyWithFilter(List(Token("were", "VBD"), Token("filmed", "VBD")),
      //     FilterWithModifier(List(Token("in", "IN")),
      //       OrValue(List(AndValue(List(NamedValue(List(Token("Germany", "NNP"))),
      //         NamedValue(List(Token("Denmark", "NNP"))))),
      //         NamedValue(List(Token("California", "NNP")))))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val movie = env.newNode()
          .out(P.isA, Q.movie)

      val germany = env.newNode()
          .out(NameLabel, "Germany")

      val denmark = env.newNode()
          .out(NameLabel, "Denmark")

      val california = env.newNode()
          .out(NameLabel, "California")

      val expected = List(movie
          .and((out(P.hasFilmingLocation, germany) and
                out(P.hasFilmingLocation, denmark))
               or out(P.hasFilmingLocation, california)))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Give/VB me/PRP all/DT musicians/NNS that/WDT "
                            + "were/VBD born/VBN in/IN Vienna/NNP "
                            + "and/CC died/VBN in/IN Berlin/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("musicians", "NNS"))),
      //   AndProperty(List(PropertyWithFilter(List(Token("were", "VBD"), Token("born", "VBN")),
      //     FilterWithModifier(List(Token("in", "IN")),
      //       NamedValue(List(Token("Vienna", "NNP"))))),
      //     PropertyWithFilter(List(Token("died", "VBN")),
      //       FilterWithModifier(List(Token("in", "IN")),
      //         NamedValue(List(Token("Berlin", "NNP")))))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val musician = env.newNode()
          .out(P.isA, Q.musician)

      val place = env.newNode()
          .out(NameLabel, "Vienna")

      val place2 = env.newNode()
          .out(NameLabel, "Berlin")

      val expected = List(musician
          .and(out(P.hasPlaceOfBirth, place))
          .and(out(P.hasPlaceOfDeath, place2)))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Which/WDT books/NN did/VBD George/NNP Orwell/NNP write/VB")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NN"))),
      //   InversePropertyWithFilter(List(Token("did", "VBD"), Token("write", "VB")),
      //     PlainFilter(NamedValue(List(Token("George", "NNP"), Token("Orwell", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expected = List(book.out(P.hasAuthor, author))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("list/VB presidents/NNS of/IN Argentina/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("presidents", "NNS"))),
      //   NamedQuery(List(Token("Argentina", "NNP"))),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val country = env.newNode()
          .out(NameLabel, "Argentina")

      val person = env.newNode()
          .in(country, P.hasHeadOfState)

      val expected = List(person)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("List/VB albums/NNS of/IN Pink/NNP Floyd/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("albums", "NNS"))),
      //   NamedQuery(List(Token("Pink", "NNP"), Token("Floyd", "NNP"))),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val artist = env.newNode()
          .out(NameLabel, "Pink Floyd")

      val album = env.newNode()
          .out(P.isA, Q.album)
          .out(P.hasPerformer, artist)

      val expected = List(album)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("List/VB the/DT actors/NNS of/IN Titanic/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("actors", "NNS"))),
      //   NamedQuery(List(Token("Titanic", "NNP"))),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val movie = env.newNode()
          .out(NameLabel, "Titanic")

      val actor = env.newNode()
          .out(P.isA, Q.actor)
          .in(movie, P.hasCastMember)

      val expected = List(actor)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Who/WP is/VBZ the/DT director/NN of/IN Big/NN Fish/NN")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("director", "NN"))),
      //   NamedQuery(List(Token("Big", "NN"), Token("Fish", "NN"))),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val movie = env.newNode()
          .out(NameLabel, "Big Fish")

      val director = env.newNode()
          .out(P.isA, Q.filmDirector)
          .in(movie, P.hasDirector)

      val expected = List(director)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("What/WP are/VBP the/DT members/NNS of/IN Metallica/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("members", "NNS"))),
      //   NamedQuery(List(Token("Metallica", "NNP"))),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val band = env.newNode()
          .out(NameLabel, "Metallica")

      val member = env.newNode()
          .out(P.isMemberOf, band)

      val expected = List(member)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("members/NNS of/IN Metallica/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("members", "NNS"))),
      //   NamedQuery(List(Token("Metallica", "NNP"))),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val band = env.newNode()
          .out(NameLabel, "Metallica")

      val member = env.newNode()
          .out(P.isMemberOf, band)

      val expected = List(member)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("What/WP is/VBZ the/DT music/NN genre/NN of/IN Gorillaz/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("music", "NN"), Token("genre", "NN"))),
      //   NamedQuery(List(Token("Gorillaz", "NNP"))),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val band = env.newNode()
          .out(NameLabel, "Gorillaz")

      val genre = env.newNode()
          .out(P.isA, Q.musicGenre)
          .in(band, P.hasGenre)

      val expected = List(genre)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("What/WP is/VBZ the/DT cast/NN of/IN Friends/NNS")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"), Token("cast", "NN"))),
      //   NamedQuery(List(Token("Friends", "NNS"))),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val show = env.newNode()
          .out(NameLabel, "Friends")

      val member = env.newNode()
          .in(show, P.hasCastMember)

      val expected = List(member)

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("who/WP are/VBP the/DT children/NNS of/IN the/DT presidents/NNS")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("children", "NNS"))),
      //   NamedQuery(List(Token("the", "DT"), Token("presidents", "NNS"))),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val president = env.newNode()
          .out(P.isA, Q.president)

      val child = env.newNode()
          .in(president, P.hasChild)

      val expected = List(child)

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("what/WP are/VBP the/DT population/NN sizes/NNS of/IN cities/NNS "
                 + "located/VBN in/IN california/NN")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("population", "NN"),
      //   Token("sizes", "NNS"))),
      //   QueryWithProperty(NamedQuery(List(Token("cities", "NNS"))),
      //     PropertyWithFilter(List(Token("located", "VBN")),
      //       FilterWithModifier(List(Token("in", "IN")),
      //         NamedValue(List(Token("california", "NN")))))),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val city = env.newNode()
          .out(P.isA, Q.city)
          .out(P.isLocatedIn,
            env.newNode()
                .out(NameLabel, "california"))

      val population = env.newNode()
          .in(city, P.hasPopulation)

      val expected = List(population)

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("Who/WP are/VBP the/DT children/NNS "
                 + "of/IN the/DT children/NNS "
                 + "of/IN Bill/NNP Clinton/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("children", "NNS"))),
      //   RelationshipQuery(NamedQuery(List(Token("the", "DT"), Token("children", "NNS"))),
      //     NamedQuery(List(Token("Bill", "NNP"), Token("Clinton", "NNP"))),
      //     Token("of", "IN")),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val bill = env.newNode()
          .out(NameLabel, "Bill Clinton")

      val child = env.newNode()
          .in(bill, P.hasChild)

      val grandChild = env.newNode()
          .in(child, P.hasChild)

      val expected = List(grandChild)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Clinton/NNP 's/POS daughters/NN")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("daughters", "NN"))),
      //   NamedQuery(List(Token("Clinton", "NNP"))),
      //   Token("'s", "POS")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val bill = env.newNode()
          .out(NameLabel, "Clinton")

      val child = env.newNode()
          .out(P.hasGender, Q.female)
          .in(bill, P.hasChild)

      val expected = List(child)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("What/WP are/VBP the/DT largest/JJS cities/NNS of/IN California/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("largest", "JJS"), Token("cities", "NNS"))),
      //   NamedQuery(List(Token("California", "NNP"))),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val state = env.newNode()
          .out(NameLabel, "California")

      val population = env.newNode()
          .aggregate(Max)

      val city = env.newNode()
          .out(P.isA, Q.city)
          .out(P.isLocatedIn, state)
          .out(P.hasPopulation, population)

      val expected = List(city)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("What/WP are/VBP the/DT biggest/JJS cities/NNS of/IN California/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("biggest", "JJS"), Token("cities", "NNS"))),
      //   NamedQuery(List(Token("California", "NNP"))),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val state = env.newNode()
          .out(NameLabel, "California")

      val population = env.newNode()
          .aggregate(Max)

      val city = env.newNode()
          .out(P.isA, Q.city)
          .out(P.isLocatedIn, state)
          .out(P.hasPopulation, population)

      val expected = List(city)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("What/WP are/VBP California/NNP 's/POS biggest/JJS cities/NNS")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("biggest", "JJS"),
      //   Token("cities", "NNS"))),
      //   NamedQuery(List(Token("California", "NNP"))),
      //   Token("'s", "POS")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val state = env.newNode()
          .out(NameLabel, "California")

      val population = env.newNode()
          .aggregate(Max)

      val city = env.newNode()
          .out(P.isA, Q.city)
          .out(P.isLocatedIn, state)
          .out(P.hasPopulation, population)

      val expected = List(city)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Who/WP is/VBZ Bill/NNP Clinton/NNP 's/POS daughter/NN")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("daughter", "NN"))),
      //   NamedQuery(List(Token("Bill", "NNP"), Token("Clinton", "NNP"))),
      //   Token("'s", "POS")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val bill = env.newNode()
          .out(NameLabel, "Bill Clinton")

      val daughter = env.newNode()
          .out(P.hasGender, Q.female)
          .in(bill, P.hasChild)

      val expected = List(daughter)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Who/WP is/VBZ Bill/NNP Clinton/NNP 's/POS daughter/NN 's/POS "
                            + "husband/NN 's/POS daughter/NN 's/POS grandfather/NN")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("grandfather", "NN"))),
      //   RelationshipQuery(NamedQuery(List(Token("daughter", "NN"))),
      //     RelationshipQuery(NamedQuery(List(Token("husband", "NN"))),
      //       RelationshipQuery(NamedQuery(List(Token("daughter", "NN"))),
      //         NamedQuery(List(Token("Bill", "NNP"), Token("Clinton", "NNP"))),
      //         Token("'s", "POS")),
      //       Token("'s", "POS")),
      //     Token("'s", "POS")),
      //   Token("'s", "POS")))

      val compiled = compile(result.get)

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

      val expected = List(grandFather)

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("Who/WP are/VBP the/DT daughters/NNS of/IN the/DT wife/NN of/IN "
                 + "the/DT president/NN of/IN the/DT United/NNP States/NNPS")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("daughters", "NNS"))),
      //   RelationshipQuery(NamedQuery(List(Token("the", "DT"), Token("wife", "NN"))),
      //     RelationshipQuery(NamedQuery(List(Token("the", "DT"), Token("president", "NN"))),
      //       NamedQuery(List(Token("the", "DT"),
      //         Token("United", "NNP"), Token("States", "NNPS"))),
      //       Token("of", "IN")),
      //     Token("of", "IN")),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

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

      val expected = List(daughter)

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("Who/WP is/VBZ the/DT son/NN of/IN the/DT actor/NN "
                 + "of/IN ``/`` I/PRP ,/, Robot/NNP ''/''")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"), Token("son", "NN"))),
      //   RelationshipQuery(NamedQuery(List(Token("the", "DT"), Token("actor", "NN"))),
      //     NamedQuery(List(Token("I", "PRP"), Token(",", ","), Token("Robot", "NNP"))),
      //     Token("of", "IN")),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val movie = env.newNode()
          .out(NameLabel, "I , Robot")

      val actor = env.newNode()
          .out(P.isA, Q.actor)
          .in(movie, P.hasCastMember)

      val son = env.newNode()
          .out(P.hasGender, Q.male)
          .in(actor, P.hasChild)

      val expected = List(son)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("children/NNS of/IN all/DT presidents/NNS of/IN the/DT US/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("children", "NNS"))),
      //   RelationshipQuery(NamedQuery(List(Token("all", "DT"), Token("presidents", "NNS"))),
      //     NamedQuery(List(Token("the", "DT"), Token("US", "NNP"))),
      //     Token("of", "IN")),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val country = env.newNode()
          .out(NameLabel, "the US")

      val president = env.newNode()
          .in(country, P.hasHeadOfState)

      val child = env.newNode()
          .in(president, P.hasChild)

      val expected = List(child)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("List/VB movies/NNS directed/VBN by/IN Quentin/NNP Tarantino/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("movies", "NNS"))),
      //   PropertyWithFilter(List(Token("directed", "VBN")),
      //     FilterWithModifier(List(Token("by", "IN")),
      //       NamedValue(List(Token("Quentin", "NNP"), Token("Tarantino", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val movie = env.newNode()
          .out(P.isA, Q.movie)

      val director = env.newNode()
          .out(NameLabel, "Quentin Tarantino")

      val expected = List(movie.out(P.hasDirector, director))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Which/WDT movies/NN did/VBD Mel/NNP Gibson/NNP direct/VB")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("movies", "NN"))),
      //   InversePropertyWithFilter(List(Token("did", "VBD"), Token("direct", "VB")),
      //     PlainFilter(NamedValue(List(Token("Mel", "NNP"), Token("Gibson", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val movie = env.newNode()
          .out(P.isA, Q.movie)

      val director = env.newNode()
          .out(NameLabel, "Mel Gibson")

      val expected = List(movie.out(P.hasDirector, director))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("actors/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(NamedQuery(List(Token("actors", "NNP"))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val actor = env.newNode()
          .out(P.isA, Q.actor)

      val expected = List(actor)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("what/WDT languages/NNS are/VBP spoken/VBN in/IN Switzerland/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("languages", "NNS"))),
      //   PropertyWithFilter(List(Token("are", "VBP"), Token("spoken", "VBN")),
      //     FilterWithModifier(List(Token("in", "IN")),
      //       NamedValue(List(Token("Switzerland", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val language = env.newNode()
          .out(P.isA, Q.language)

      val country = env.newNode()
          .out(NameLabel, "Switzerland")

      val expected = List(language
          .in(country, P.hasOfficialLanguage))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("Which/WDT books/NN did/VBD Orwell/NNP or/CC Shakespeare/NNP write/VB")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NN"))),
      //   InversePropertyWithFilter(List(Token("did", "VBD"), Token("write", "VB")),
      //     PlainFilter(OrValue(List(NamedValue(List(Token("Orwell", "NNP"))),
      //       NamedValue(List(Token("Shakespeare", "NNP")))))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val book = env.newNode()
          .out(P.isA, Q.book)

      val orwell = env.newNode()
          .out(NameLabel, "Orwell")

      val shakespeare = env.newNode()
          .out(NameLabel, "Shakespeare")

      val expected = List(book
          .and(out(P.hasAuthor, orwell)
               or out(P.hasAuthor, shakespeare)))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("Which/WDT books/NN were/VBD authored/VBN by/IN George/NNP Orwell/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NN"))),
      //   PropertyWithFilter(List(Token("were", "VBD"), Token("authored", "VBN")),
      //     FilterWithModifier(List(Token("by", "IN")),
      //       NamedValue(List(Token("George", "NNP"), Token("Orwell", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expected = List(book
          .out(P.hasAuthor, author))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("Which/WDT instrument/NN did/VBD John/NNP Lennon/NNP play/VB")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("instrument", "NN"))),
      //   InversePropertyWithFilter(List(Token("did", "VBD"), Token("play", "VB")),
      //     PlainFilter(NamedValue(List(Token("John", "NNP"), Token("Lennon", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val instrument = env.newNode()
          .out(P.isA, Q.musicalInstrument)

      val musician = env.newNode()
          .out(NameLabel, "John Lennon")

      val expected = List(instrument
          .in(musician, P.playsInstrument))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("Who/WP wrote/VBD ``/`` Le/NNP Petit/NNP Prince/NNP ''/'' "
                 + "and/CC ``/`` Vol/NNP de/IN Nuit/NNP ''/''")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // PersonListQuestion(PropertyWithFilter(List(Token("wrote", "VBD")),
      //   PlainFilter(AndValue(List(NamedValue(List(Token("Le", "NNP"),
      //     Token("Petit", "NNP"), Token("Prince", "NNP"))),
      //     NamedValue(List(Token("Vol", "NNP"), Token("de", "IN"), Token("Nuit", "NNP"))))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val book = env.newNode()
          .out(NameLabel, "Le Petit Prince")

      val book2 = env.newNode()
          .out(NameLabel, "Vol de Nuit")

      val expected = List(person
          .and(in(book, P.hasAuthor))
          .and(in(book2, P.hasAuthor)))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("What/WP did/VBD George/NNP Orwell/NNP write/VB")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ThingListQuestion(InversePropertyWithFilter(List(Token("did", "VBD"),
      //   Token("write", "VB")),
      //   PlainFilter(NamedValue(List(Token("George", "NNP"), Token("Orwell", "NNP"))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val thing = env.newNode()

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expected = List(thing
          .out(P.hasAuthor, author))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("What/WP was/VBD authored/VBN by/IN George/NNP Orwell/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ThingListQuestion(PropertyWithFilter(List(Token("was", "VBD"), Token("authored", "VBN")),
      //   FilterWithModifier(List(Token("by", "IN")),
      //     NamedValue(List(Token("George", "NNP"), Token("Orwell", "NNP"))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val thing = env.newNode()

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expected = List(thing
          .out(P.hasAuthor, author))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("What/WP books/NNP were/VBD authored/VBN by/IN George/NNP Orwell/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NNP"))),
      //   PropertyWithFilter(List(Token("were", "VBD"), Token("authored", "VBN")),
      //     FilterWithModifier(List(Token("by", "IN")),
      //       NamedValue(List(Token("George", "NNP"), Token("Orwell", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expected = List(book
          .out(P.hasAuthor, author))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("authors/NNS who/WP died/VBD")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("authors", "NNS"))),
      //   NamedProperty(List(Token("died", "VBD")))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val author = env.newNode()
          .in(env.newNode(), P.hasAuthor)

      val place = env.newNode()

      val expected = List(author
          .and(out(P.hasDateOfDeath, place)
               or out(P.hasPlaceOfDeath, place)))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("authors/NNS which/WDT died/VBD in/IN Berlin/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("authors", "NNS"))),
      //   PropertyWithFilter(List(Token("died", "VBD")),
      //     FilterWithModifier(List(Token("in", "IN")),
      //       NamedValue(List(Token("Berlin", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val author = env.newNode()
          .in(env.newNode(), P.hasAuthor)

      val place = env.newNode()
          .out(NameLabel, "Berlin")

      val expected = List(author
          .out(P.hasPlaceOfDeath, place))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("What/WDT actor/NN married/VBD John/NNP F./NNP Kennedy/NNP 's/POS sister/NN")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      //        ListQuestion(QueryWithProperty(NamedQuery(List(Token("actor", "NN"))),
      //          PropertyWithFilter(List(Token("married", "VBD")),
      //            PlainFilter(ValueRelationship(NamedValue(List(Token("sister", "NN"))),
      //              NamedValue(List(Token("John", "NNP"), Token("F.", "NNP"),
      //                Token("Kennedy", "NNP"))))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val actor = env.newNode()
          .out(P.isA, Q.actor)

      val kennedy = env.newNode()
          .out(NameLabel, "John F. Kennedy")

      val sister = env.newNode()
          .in(kennedy, P.hasSister)

      val expected = List(actor.out(P.hasSpouse, sister))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Who/WP did/VBD Bill/NNP Clinton/NNP 's/POS daughter/NN marry/VB")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // PersonListQuestion(InversePropertyWithFilter(List(Token("did", "VBD"),
      //   Token("marry", "VB")),
      //   PlainFilter(ValueRelationship(NamedValue(List(Token("daughter", "NN"))),
      //     NamedValue(List(Token("Bill", "NNP"), Token("Clinton", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val bill = env.newNode()
          .out(NameLabel, "Bill Clinton")

      val daughter = env.newNode()
          .out(P.hasGender, Q.female)
          .in(bill, P.hasChild)

      val expected = List(person.out(P.hasSpouse, daughter))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Clinton/NNP 's/POS children/NNS and/CC grandchildren/NNS")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("children", "NNS"))),
      //   NamedQuery(List(Token("grandchildren", "NNS"))))),
      //   NamedQuery(List(Token("Clinton", "NNP"))),
      //   Token("'s", "POS")))

      val compiled = compile(result.get)

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

      val expected = List(child, grandchild)

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("What/WP are/VBP the/DT population/NN of/IN China/NNP and/CC the/DT USA/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("population", "NN"))),
      //   AndQuery(List(NamedQuery(List(Token("China", "NNP"))),
      //     NamedQuery(List(Token("the", "DT"), Token("USA", "NNP"))))),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val china = env.newNode()
          .out(NameLabel, "China")

      val usa = env.newNode()
          .out(NameLabel, "the USA")

      val populationOfChina = env.newNode().in(china, P.hasPopulation)

      val populationOfUSA = env.newNode().in(usa, P.hasPopulation)

      val expected = List(populationOfChina, populationOfUSA)

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("the/DT population/NNP of/IN Japan/NNP and/CC China/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"),
      //   Token("population", "NNP"))),
      //   AndQuery(List(NamedQuery(List(Token("Japan", "NNP"))),
      //     NamedQuery(List(Token("China", "NNP"))))),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val japan = env.newNode()
          .out(NameLabel, "Japan")

      val china = env.newNode()
          .out(NameLabel, "China")

      val populationOfJapan = env.newNode().in(japan, P.hasPopulation)

      val populationOfChina = env.newNode().in(china, P.hasPopulation)

      val expected = List(populationOfJapan, populationOfChina)

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("the/DT population/NNP and/CC area/NNP of/IN Japan/NNP and/CC China/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("the", "DT"),
      //   Token("population", "NNP"))),
      //   NamedQuery(List(Token("area", "NNP"))))),
      //   AndQuery(List(NamedQuery(List(Token("Japan", "NNP"))),
      //     NamedQuery(List(Token("China", "NNP"))))),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val japan = env.newNode()
          .out(NameLabel, "Japan")

      val china = env.newNode()
          .out(NameLabel, "China")

      val populationOfJapan = env.newNode().in(japan, P.hasPopulation)

      val populationOfChina = env.newNode().in(china, P.hasPopulation)

      val areaOfJapan = env.newNode().in(japan, P.hasArea)

      val areaOfChina = env.newNode().in(china, P.hasArea)

      val expected = List(populationOfJapan,populationOfChina,
        areaOfJapan, areaOfChina)

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("the/DT population/NN ,/, land/NN area/NN and/CC capitals/NNP "
                 + "of/IN Japan/NNP ,/, India/NNP and/CC China/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("the", "DT"),
      //   Token("population", "NN"))),
      //   NamedQuery(List(Token("land", "NN"), Token("area", "NN"))),
      //   NamedQuery(List(Token("capitals", "NNP"))))),
      //   AndQuery(List(NamedQuery(List(Token("Japan", "NNP"))),
      //     NamedQuery(List(Token("India", "NNP"))),
      //     NamedQuery(List(Token("China", "NNP"))))),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val japan = env.newNode()
          .out(NameLabel, "Japan")

      val india = env.newNode()
          .out(NameLabel, "India")

      val china = env.newNode()
          .out(NameLabel, "China")

      val populationOfJapan = env.newNode().in(japan, P.hasPopulation)

      val populationOfIndia = env.newNode().in(india, P.hasPopulation)

      val populationOfChina = env.newNode().in(china, P.hasPopulation)

      val areaOfJapan = env.newNode().in(japan, P.hasArea)

      val areaOfIndia = env.newNode().in(india, P.hasArea)

      val areaOfChina = env.newNode().in(china, P.hasArea)

      val capitalOfJapan = env.newNode().in(japan, P.hasCapital)

      val capitalOfIndia = env.newNode().in(india, P.hasCapital)

      val capitalOfChina = env.newNode().in(china, P.hasCapital)

      val expected = List(populationOfJapan, populationOfIndia, populationOfChina,
        areaOfJapan, areaOfIndia, areaOfChina,
        capitalOfJapan, capitalOfIndia, capitalOfChina)

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("Japan/NNP and/CC China/NNP 's/POS population/NNP and/CC area/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("population", "NNP"))),
      //   NamedQuery(List(Token("area", "NNP"))))),
      //   AndQuery(List(NamedQuery(List(Token("Japan", "NNP"))),
      //     NamedQuery(List(Token("China", "NNP"))))),
      //   Token("'s", "POS")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val japan = env.newNode()
          .out(NameLabel, "Japan")

      val china = env.newNode()
          .out(NameLabel, "China")

      val populationOfJapan = env.newNode().in(japan, P.hasPopulation)

      val populationOfChina = env.newNode().in(china, P.hasPopulation)

      val areaOfJapan = env.newNode().in(japan, P.hasArea)

      val areaOfChina = env.newNode().in(china, P.hasArea)

      val expected = List(populationOfJapan, populationOfChina,
        areaOfJapan, areaOfChina)

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("children/NNS and/CC grandchildren/NNS of/IN Clinton/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(RelationshipQuery(AndQuery(List(NamedQuery(List(Token("children", "NNS"))),
      //   NamedQuery(List(Token("grandchildren", "NNS"))))),
      //   NamedQuery(List(Token("Clinton", "NNP"))),
      //   Token("of", "IN")))

      val compiled = compile(result.get)

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

      val expected = List(child, grandchild)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Who/WP wrote/VBD books/NNS")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // PersonListQuestion(PropertyWithFilter(List(Token("wrote", "VBD")),
      //     PlainFilter(NamedValue(List(Token("books", "NNS"))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val book = env.newNode()
          .out(P.isA, Q.book)

      val expected = List(person.in(book, P.hasAuthor))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("presidents/NNS that/WDT have/VBP children/NNS")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("presidents", "NNS"))),
      //   PropertyWithFilter(List(Token("have", "VBP")),
      //     PlainFilter(NamedValue(List(Token("children", "NNS")))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val president = env.newNode()
          .out(P.isA, Q.president)

      val child = env.newNode()
          .in(env.newNode(), P.hasChild)

      val expected = List(president.out(P.hasChild, child))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("what/WP are/VBP the/DT largest/JJS cities/NNS in/IN europe/NN")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(
      //   NamedQuery(List(Token("the", "DT"), Token("largest", "JJS"), Token("cities", "NNS"))),
      //   PropertyWithFilter(List(),
      //     FilterWithModifier(List(Token("in", "IN")),
      //       NamedValue(List(Token("europe", "NN")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val city = env.newNode()
          .out(P.isA, Q.city)

      val population = env.newNode()
          .aggregate(Max)

      val europe = env.newNode()
          .out(NameLabel, "europe")

      val expected = List(city
          .out(P.hasPopulation, population)
          .out(P.isLocatedIn, europe))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("List/VB books/NNS by/IN George/NNP Orwell/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NNS"))),
      //   PropertyWithFilter(List(),
      //     FilterWithModifier(List(Token("by", "IN")),
      //       NamedValue(List(Token("George", "NNP"), Token("Orwell", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "George Orwell")

      val expected = List(book.out(P.hasAuthor, author))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("Which/WDT city/NN is/VBZ bigger/JJR than/IN New/NNP York/NNP City/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("city", "NN"))),
      //   PropertyWithFilter(List(Token("is", "VBZ")),
      //     FilterWithComparativeModifier(List(Token("bigger", "JJR"), Token("than", "IN")),
      //       NamedValue(List(Token("New", "NNP"), Token("York", "NNP"), Token("City", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val city = env.newNode()
          .out(P.isA, Q.city)

      val newYorkCity = env.newNode()
          .out(NameLabel, "New York City")

      val otherArea = env.newNode()
          .in(newYorkCity, P.hasArea)

      val area = env.newNode()
          .filter(GreaterThanFilter(otherArea))

      val expected = List(city.out(P.hasArea, area))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Who/WP is/VBZ older/JJR than/IN Obama/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // PersonListQuestion(PropertyWithFilter(List(Token("is", "VBZ")),
      //   FilterWithComparativeModifier(List(Token("older", "JJR"), Token("than", "IN")),
      //     NamedValue(List(Token("Obama", "NNP"))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val person = env.newNode()
          .out(P.isA, Q.human)

      val obama = env.newNode()
          .out(NameLabel, "Obama")

      val otherBirthDate = env.newNode()
          .in(obama, P.hasDateOfBirth)

      val birthDate = env.newNode()
          .filter(LessThanFilter(otherBirthDate))

      val expected = List(person.out(P.hasDateOfBirth, birthDate))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("which/WDT mountains/NNS are/VBP more/JJR than/IN 1000/CD meters/NNS high/JJ")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("mountains", "NNS"))),
      //   AdjectivePropertyWithFilter(List(Token("are", "VBP"), Token("high", "JJ")),
      //     FilterWithComparativeModifier(List(Token("more", "JJR"), Token("than", "IN")),
      //       NumberWithUnit(List(Token("1000", "CD")), List(Token("meters", "NNS")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val mountain = env.newNode()
          .out(P.isA, Q.mountain)

      val minElevation: WikidataNode = (1000.0, U.meter)

      val elevation = env.newNode()
          .filter(GreaterThanFilter(minElevation))

      val expected = List(mountain.out(P.hasElevation, elevation))

      assertEquals(expected, compiled)
    }
    {
      val tokens =
        tokenize("Which/WDT cities/NNS have/VBP more/JJR than/IN "
                 + "two/CD million/CD inhabitants/NNS")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("cities", "NNS"))),
      //   PropertyWithFilter(List(Token("have", "VBP")),
      //     FilterWithComparativeModifier(List(Token("more", "JJR"), Token("than", "IN")),
      //       NumberWithUnit(List(Token("two", "CD"), Token("million", "CD")),
      //         List(Token("inhabitants", "NNS")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val city = env.newNode()
          .out(P.isA, Q.city)

      val minPopulation: WikidataNode = 2000000.0

      val population = env.newNode()
          .filter(GreaterThanFilter(minPopulation))

      val expected = List(city.out(P.hasPopulation, population))

      assertEquals(expected, compiled)
    }
    // TODO: adjective "californian": P.isLocatedIn
    {
      val tokens = tokenize("In/IN which/WDT californian/JJ cities/NNS "
                            + "live/VBP more/JJR than/IN 2/CD million/CD people/NNS")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("californian", "NN"),
      //   Token("cities", "NNS"))),
      //   PropertyWithFilter(List(Token("live", "VBP")),
      //     FilterWithComparativeModifier(List(Token("more", "JJR"), Token("than", "IN")),
      //       NumberWithUnit(List(Token("2", "CD"), Token("million", "CD")),
      //         List(Token("people", "NNS")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val city = env.newNode()
          .out(P.isA, Q.city)

      val minPopulation: WikidataNode = 2000000.0

      val population = env.newNode()
          .filter(GreaterThanFilter(minPopulation))

      val expected = List(city.out(P.hasPopulation, population))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Who/WP is/VBD the/DT discoverer/NNS of/IN Pluto/NNP")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val pluto = env.newNode()
          .out(NameLabel, "Pluto")

      val discoverer = env.newNode()
          .in(pluto, P.hasDiscovererOrInventor)

      val expected = List(discoverer)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Who/WP discovered/VBD the/DT most/JJS planets/NNS")
      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // PersonListQuestion(PropertyWithFilter(List(Token("discovered", "VBD")),
      //   PlainFilter(NamedValue(List(Token("the", "DT"), Token("most", "JJS"),
      //     Token("planets", "NNS"))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val discoverer = env.newNode()
          .out(P.isA, Q.human)

      val planet = env.newNode()
          .out(P.isA, Q.planet)
          .aggregate(Count)
          .aggregate(Max)

      val expected = List(discoverer
          .in(planet, P.hasDiscovererOrInventor))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Who/WP are/VBP the/DT children/NNS of/IN Clinton/NNP 's/POS spouse/NNS")

      val result = parseListQuestion(tokens)

      assertSuccess(result)

      // ListQuestion(RelationshipQuery(NamedQuery(List(Token("the", "DT"), Token("children", "NNS"))),
      //   RelationshipQuery(NamedQuery(List(Token("spouse", "NNS"))),
      //     NamedQuery(List(Token("Clinton", "NNP"))), Token("'s", "POS")), Token("of", "IN")))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val clinton = env.newNode()
          .out(NameLabel, "Clinton")

      val spouse = env.newNode()
          .in(clinton, P.hasSpouse)

      val child = env.newNode()
          .in(spouse, P.hasChild)

      val expected = List(child)

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Which/WDT books/NNS were/VBD written/VBN by/IN Jane/NNP Austen/NNP")

      val result = parseListQuestion(tokens)

      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("books", "NNS"))),
      //   PropertyWithFilter(List(Token("were", "VBD"), Token("written", "VBN")),
      //     FilterWithModifier(List(Token("by", "IN")),
      //       NamedValue(List(Token("Jane", "NNP"), Token("Austen", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val book = env.newNode()
          .out(P.isA, Q.book)

      val author = env.newNode()
          .out(NameLabel, "Jane Austen")

      val expected = List(book.out(P.hasAuthor, author))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Which/WDT country/NN was/VBD Obama/NNP born/VBN in/IN")

      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("country", "NN"))),
      //   InversePropertyWithFilter(List(Token("was", "VBD"), Token("born", "VBN"),
      //     Token("in", "IN")),
      //     PlainFilter(NamedValue(List(Token("Obama", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val country = env.newNode()
          .out(P.isA, Q.country)

      val obama = env.newNode()
          .out(NameLabel, "Obama")

      val place = env.newNode()
          .in(obama, P.hasPlaceOfBirth)

      val expected = List(country
          .in(place, P.country))

      assertEquals(expected, compiled)
    }
    {
      val tokens = tokenize("Which/WDT year/NN was/VBD Obama/NNP born/VBN in/IN")

      val result = parseListQuestion(tokens)
      assertSuccess(result)

      // ListQuestion(QueryWithProperty(NamedQuery(List(Token("year", "NN"))),
      //   InversePropertyWithFilter(List(Token("was", "VBD"), Token("born", "VBN"),
      //     Token("in", "IN")),
      //     PlainFilter(NamedValue(List(Token("Obama", "NNP")))))))

      val compiled = compile(result.get)

      val env = new WikidataEnvironment()

      val year = env.newNode()
          .out(P.isA, Q.year)

      val obama = env.newNode()
          .out(NameLabel, "Obama")

      val date = env.newNode()
          .in(obama, P.hasDateOfBirth)

      val expected = List(year
          .in(date, YearLabel))

      assertEquals(expected, compiled)
    }
  }
}