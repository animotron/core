package org.animotron.instruction.ml;

import org.animotron.Properties;
import org.animotron.graph.AnimoGraph;
import org.animotron.instruction.AbstractInstruction;
import org.animotron.instruction.InstructionContainer;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

public abstract class MLInstruction extends AbstractInstruction {
	
	private static final InstructionContainer CONTAINER = ML.getInstance();

	public MLInstruction(String name) {
		super(name, CONTAINER.name(), CONTAINER.namespace());
	}
	
	public Node build(Node parent, String ns, String name){
		return build(parent, AnimoGraph.createNode(), ns, name);
	}
	
	public Node build(Node parent, Node child, String ns, String name){
		Relationship relationship = parent.createRelationshipTo(child, relationshipType());
		Properties.NAMESPACE.set(relationship, ns);
		Properties.NAME.set(relationship, name);
		return child;
	}

}
