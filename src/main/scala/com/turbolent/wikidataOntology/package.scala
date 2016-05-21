package com.turbolent.wikidataOntology

import scala.language.implicitConversions

import java.time.temporal.Temporal

import com.turbolent.questionCompiler.EdgeContext
import com.turbolent.questionCompiler.graph._


object `package` {

  type WikidataNode = Node[NodeLabel, EdgeLabel]
  type WikidataEdge = Edge[EdgeLabel, NodeLabel]
  type WikidataFilter = Filter[NodeLabel, EdgeLabel]

  implicit def propertyAsLabel(property: Property): EdgeLabel =
    PropertyLabel(property)

  implicit def itemAsNode(item: Item): WikidataNode =
    Node(ItemLabel(item))

  implicit def stringAsNode(string: String): WikidataNode =
    Node(ValueLabel(string))

  implicit def doubleAsNode(number: Double): WikidataNode =
    Node(NumberLabel(number))

  implicit def tupleAsNode(tuple: (Double, Unit)): WikidataNode = {
    val (number, unit) = tuple
    Node(NumberWithUnitLabel(number, unit))
  }

  implicit def temporalAsNode(temporal: Temporal): WikidataNode =
    Node(TemporalLabel(temporal))

  def out(label: EdgeLabel, target: WikidataNode) =
    OutEdge(label, target)

  def in(source: WikidataNode, label: EdgeLabel) =
    InEdge(source, label)

  type NodeFactory = (WikidataNode, WikidataEnvironment) => WikidataNode
  type EdgeFactory = (WikidataNode, WikidataEnvironment) => WikidataEdge
  type ContextfulEdgeFactory = (WikidataNode, EdgeContext, WikidataEnvironment) => WikidataEdge

  implicit def asNodeFactory(item: Item): NodeFactory =
    (node, env) => node.out(P.isA, item)

  implicit def asContextfulEdgeFactory(property: Property): ContextfulEdgeFactory =
    (node, context, env) => out(property, node)

  implicit def asEdgeFactory(property: Property): EdgeFactory =
    (node, env) => out(property, node)

  def reverse(property: Property): EdgeFactory =
    (node, env) => in(node, property)

  def contextfulReverse(property: Property): ContextfulEdgeFactory =
    (node, context, env) => in(node, property)

}