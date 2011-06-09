package org.animotron;

import org.animotron.graph.GraphBuilder;
import org.animotron.instruction.ml.ValueInstruction;
import org.neo4j.graphdb.Relationship;

public class Expression extends GraphBuilder {
	
	Object[] e;
	
	public Expression(Object[] e) {
		this.e = e;
	}
	
	public Relationship build() {
		startGraph();
		build(e);
		endGraph();
		return getRelationship();
	}

	private void build(Object[] e) {

		Statement statement = Statements.clazz((Class<? extends Statement>) e[0]);
		
		String prefix, ns, name, value;
		Object[] child = null;
		
		if (statement instanceof ValueInstruction) {
			prefix = ns = name  = null;
			value = (String) e[1];
		} else if (statement instanceof ValueInstruction) {
			
		}
		
		start(statement, prefix, ns, name, value);
		build(child);
		end();
		
	}

}
