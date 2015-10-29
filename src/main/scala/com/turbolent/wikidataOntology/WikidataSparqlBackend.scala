package com.turbolent.wikidataOntology

import java.time.Year
import java.util.concurrent.atomic.AtomicInteger

import com.turbolent.questionCompiler.sparql.SparqlBackend
import org.apache.jena.datatypes.RDFDatatype
import org.apache.jena.datatypes.xsd.XSDDatatype
import org.apache.jena.graph.{NodeFactory => JenaNodeFactory, Node => JenaNode}
import org.apache.jena.query.Query
import org.apache.jena.sparql.core.Var


object WikidataSparqlBackend extends SparqlBackend[NodeLabel, EdgeLabel] {

  val ENTITY_BASE = "http://www.wikidata.org/entity/"
  val PROPERTY_BASE = "http://www.wikidata.org/prop/direct/"
  val RDFS_BASE = "http://www.w3.org/2000/01/rdf-schema#"
  val XSD_BASE = "http://www.w3.org/2001/XMLSchema#"


  override def prepareQuery(query: Query) {
    query.setPrefix("wd", ENTITY_BASE)
    query.setPrefix("wdt", PROPERTY_BASE)
    query.setPrefix("rdfs", RDFS_BASE)
    query.setPrefix("xsd", XSD_BASE)
  }

  def compileUnit(unit: Unit): RDFDatatype =
  // TODO:
    XSDDatatype.XSDinteger

  override def compileNodeLabel(label: NodeLabel): JenaNode =
    label match {
      case VarLabel(id) =>
        Var.alloc(id.toString)

      case ItemLabel(item) =>
        JenaNodeFactory.createURI(ENTITY_BASE + "Q" + item.id)

      case ValueLabel(value) =>
        JenaNodeFactory.createLiteral(value, "en")

      case NumberLabel(value) =>
        JenaNodeFactory.createLiteral(value.toString, XSDDatatype.XSDdouble)

      case NumberWithUnitLabel(value, unit) =>
        val datatype = compileUnit(unit)
        JenaNodeFactory.createLiteral(value.toString, datatype)

      case TemporalLabel(temporal) =>
        temporal match {
          case year: Year =>
            val value = year.getValue.toString
            JenaNodeFactory.createLiteral(value, XSDDatatype.XSDinteger)
        }
    }

  val nestedProperties = Set(P.isLocatedIn)

  def compileProperty(property: Property): JenaNode = {
    val node = JenaNodeFactory.createURI(PROPERTY_BASE + "P" + property.id)

    // TODO:
//    if (nestedProperties.contains(property)) {
//      val link = PathFactory.pathLink(node)
//      val path = PathFactory.pathOneOrMore1(link)
    node
  }

  override def compileEdgeLabel(label: EdgeLabel): JenaNode =
    label match {
      case PropertyLabel(property) =>
        compileProperty(property)

      case NameLabel =>
        JenaNodeFactory.createURI(RDFS_BASE + "label")

    }

  val nextVariableID = new AtomicInteger(0)

  override def makeAnonymousVariable(): JenaNode =
    Var.alloc("_a" + nextVariableID.getAndIncrement())
}
