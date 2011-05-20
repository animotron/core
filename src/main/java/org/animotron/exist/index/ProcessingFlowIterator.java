package org.animotron.exist.index;

import java.util.Iterator;

import org.animotron.graph.RelationshipTypes;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;

public class ProcessingFlowIterator implements Iterator<Node> {
	
	private final Node the;
	private Iterator<Node> it;
	
	public ProcessingFlowIterator(Node the) {
		this.the = the;
		TraversalDescription td = 
			Traversal.description().
				breadthFirst().
				relationships(RelationshipTypes.ELEMENT, Direction.OUTGOING );
				//.evaluator(Evaluators.excludeStartPosition());
	
		it = td.traverse(this.the).nodes().iterator();
	}

	@Override
	public boolean hasNext() {
		return it.hasNext();
	}

	@Override
	public Node next() {
		Node node = it.next();
		//short type = (Short) node.getProperty(KEY_NODE_TYPE);
		//if (type == 1) {
		//	return new THE(node);
		//}else if (type == 11) {
		//	return new HAVE(node);
		//}
		
		return node;
	}

	@Override
	public void remove() {
		throw new RuntimeException("Read only iterator.");
	}
	
}