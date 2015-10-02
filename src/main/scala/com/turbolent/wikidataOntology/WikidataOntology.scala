package com.turbolent.wikidataOntology

import com.turbolent.questionCompiler.Ontology


object WikidataOntology extends Ontology[NodeLabel, EdgeLabel, WikidataEnvironment]
    with InversePropertyEdgeFactory
    with RelationshipEdgeFactory
    with NumberNodeFactory
    with AdjectivePropertyEdgeFactory
    with ComparativePropertyEdgeFactory
    with ValuePropertyEdgeFactory
    with ValueNodeFactory
    with PersonEdgeFactory
    with NamedPropertyEdgeFactory
