package com.turbolent.wikidataOntology

import java.time.Year
import java.util.concurrent.atomic.AtomicInteger

import com.turbolent.questionCompiler.sparql.SparqlBackend
import org.apache.jena.datatypes.RDFDatatype
import org.apache.jena.datatypes.xsd.XSDDatatype
import org.apache.jena.graph.{NodeFactory => JenaNodeFactory, Node => JenaNode, Triple => JenaTriple}
import org.apache.jena.query.Query
import org.apache.jena.sparql.algebra.Op
import org.apache.jena.sparql.algebra.op.{OpBGP, OpJoin, OpService}
import org.apache.jena.sparql.core.{Var, BasicPattern}
import org.apache.jena.sparql.path._


object WikidataSparqlBackend extends SparqlBackend[NodeLabel, EdgeLabel] {

  val ENTITY_BASE = "http://www.wikidata.org/entity/"
  val PROPERTY_BASE = "http://www.wikidata.org/prop/direct/"
  val RDFS_BASE = "http://www.w3.org/2000/01/rdf-schema#"
  val XSD_BASE = "http://www.w3.org/2001/XMLSchema#"
  val WIKIBASE_BASE = "http://wikiba.se/ontology#"
  val BIGDATA_BASE = "http://www.bigdata.com/rdf#"
  val STATEMENT_BASE = "http://www.wikidata.org/prop/"
  val VALUE_BASE = "http://www.wikidata.org/prop/statement/"

  override def prepareQuery(query: Query) {
    query.setPrefix("wd", ENTITY_BASE)
    query.setPrefix("wdt", PROPERTY_BASE)
    query.setPrefix("rdfs", RDFS_BASE)
    query.setPrefix("xsd", XSD_BASE)
    query.setPrefix("wikibase", WIKIBASE_BASE)
    query.setPrefix("bd", BIGDATA_BASE)
    query.setPrefix("p", STATEMENT_BASE)
    query.setPrefix("v", VALUE_BASE)
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

  def compilePropertyNode(property: Property) =
    JenaNodeFactory.createURI(PROPERTY_BASE + "P" + property.id)

  def compilePropertyStatementNode(property: Property) =
    JenaNodeFactory.createURI(STATEMENT_BASE + "P" + property.id)

  def compilePropertyValueNode(property: Property) =
    JenaNodeFactory.createURI(VALUE_BASE + "P" + property.id)


  val instanceOfPath = {
    // use `p:P31/v:P31/wdt:P279*` instead of plain wdt:P31:
    //   - entities will may have several values for this property and they are only
    //     accessible through the statement/value path
    //   - instance relationships through superclasses are not automatically inferred

    val instanceOfStatementPath = new P_Link(compilePropertyStatementNode(P.isA))
    val instanceOfValuePath = new P_Link(compilePropertyValueNode(P.isA))
    val subclassOfPath = new P_Link(compilePropertyNode(P.isSubclassOf))
    new P_Seq(instanceOfStatementPath,
      new P_Seq(instanceOfValuePath,
        new P_ZeroOrMore1(subclassOfPath)))
  }

  val transitiveProperties = Set(P.isLocatedIn)

  def compileProperty(property: Property): Either[JenaNode, Path] = {
    val node = compilePropertyNode(property)

    if (property == P.isA) {
      Right(instanceOfPath)
    } else if (transitiveProperties.contains(property)) {
      val link = new P_Link(node)
      val path = new P_OneOrMore1(link)
      Right(path)
    } else
      Left(node)
  }

  override def compileEdgeLabel(label: EdgeLabel): Either[JenaNode, Path] =
    label match {
      case PropertyLabel(property) =>
        compileProperty(property)

      case NameLabel =>
        Left(JenaNodeFactory.createURI(RDFS_BASE + "label"))

    }

  val nextVariableID = new AtomicInteger(0)

  override def makeAnonymousVariable(): JenaNode =
    Var.alloc("_a" + nextVariableID.getAndIncrement())

  override def additionalResultVariables(variable: Var) = {
    // add additional label variable, resolved by labeling service.
    // see prepareOp
    val name = variable.getVarName
    val labelVariable = Var.alloc(name + "Label")
    List(labelVariable)
  }

  override def prepareOp(op: Op) = {
    // enable labeling service by adding
    // `SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }`.
    // see also additionalResultVariables

    val labelNode = JenaNodeFactory.createURI(WIKIBASE_BASE + "label")

    val triple = new JenaTriple(JenaNodeFactory.createURI(BIGDATA_BASE + "serviceParam"),
      JenaNodeFactory.createURI(WIKIBASE_BASE + "language"),
      JenaNodeFactory.createLiteral("en"))
    val pattern = new BasicPattern()
    pattern.add(triple)
    val subOp = new OpBGP(pattern)

    val serviceOp = new OpService(labelNode, subOp, false)
    OpJoin.create(op, serviceOp)
  }
}
