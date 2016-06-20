package com.turbolent.wikidataOntology

import com.turbolent.questionCompiler.graph.{GreaterThanFilter, LessThanFilter, Node}
import org.scalatest.FunSuite


class SPARQLSuite extends FunSuite with Utilities {

  test("discoverers of Pluto and Nix") {
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

    val expectedQuery = parseSparqlQuery("3",
      """
        |{ ?3 p:P31/(v:P31/(wdt:P279)*) wd:Q5
        |    { ?1  wdt:P61     ?3 ;
        |          rdfs:label  "Pluto"@en
        |    }
        |  UNION
        |    { ?2  wdt:P61     ?3 ;
        |          rdfs:label  "Nix"@en
        |    }
        |}
      """.stripMargin,
      None)

    assertEquivalent(expectedQuery, actualQuery)
  }


  test("cast members of Alien") {
    val env = new WikidataEnvironment()
    val person = env.newNode()
        .out(P.isA, Q.human)

    val movie = env.newNode()
        .out(NameLabel, "Alien")

    val root = person.in(movie, P.hasCastMember)

    val actualQuery = compileSparqlQuery(root, env)

    val expectedQuery = parseSparqlQuery("1",
      """
        |{ ?1 p:P31/(v:P31/(wdt:P279)*) wd:Q5
        |  { ?2  wdt:P161    ?1 ;
        |        rdfs:label  "Alien"@en
        |  }
        |}
      """.stripMargin,
      None)

    assertEquivalent(expectedQuery, actualQuery)
  }


  test("mountains higher than 1000 meters") {
    val env = new WikidataEnvironment()
    val elevation: WikidataNode = (1000.0, U.meter)

    val root = env.newNode()
        .out(P.isA, Q.mountain)
        .out(P.hasElevation, elevation)

    val actualQuery = compileSparqlQuery(root, env)

    val expectedQuery = parseSparqlQuery("1",
      """
        |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q8502
        |  { ?1  wdt:P2044   "1000.0"^^<http://www.w3.org/2001/XMLSchema#integer> }
        |}
      """.stripMargin,
      None)

    assertEquivalent(expectedQuery, actualQuery)
  }


  test("filters") {
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

    val expectedQuery = parseSparqlQuery("4",
      """
        |{ ?4  wdt:P2046  ?3 .
        |  ?1  wdt:P2046  ?2
        |  FILTER ( ?2 > "23.0"^^xsd:double )
        |  FILTER ( ?3 < ?2 )
        |}
      """.stripMargin,
      None)

    assertEquivalent(expectedQuery, actualQuery)
  }


  test("uncompilable") {
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


  test("cities larger than New York City") {
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

    val expectedQuery = parseSparqlQuery("1",
      """
        |{ ?1  p:P31/(v:P31/(wdt:P279)*)  wd:Q515
        |  { { ?1  wdt:P2046  ?4 .
        |      ?2  wdt:P2046  ?3 .
        |      ?2  rdfs:label  "New York City"@en .
        |      }
        |    FILTER ( ?4 > ?3 )
        |  }
        |}
      """.stripMargin,
      None)

    assertEquivalent(expectedQuery, actualQuery)
  }


  test("multiple filters") {
    val env = new WikidataEnvironment()
    val min: WikidataNode = Node(NumberLabel(23))

    val max: WikidataNode = Node(NumberLabel(42))

    val area = env.newNode()
        .filter(GreaterThanFilter(min)
                and LessThanFilter(max))

    val root = env.newNode()
        .out(P.hasArea, area)

    val actualQuery = compileSparqlQuery(root, env)

    val expectedQuery = parseSparqlQuery("2",
      """
        |{ ?2  wdt:P2046  ?1 .
        |  FILTER ( ?1 > "23.0"^^xsd:double )
        |  FILTER ( ?1 < "42.0"^^xsd:double )
        |}
      """.stripMargin,
      None)

    assertEquivalent(expectedQuery, actualQuery)
  }

}
