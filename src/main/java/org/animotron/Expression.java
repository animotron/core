package org.animotron;

import org.animotron.graph.GraphBuilder;
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

	@SuppressWarnings("unchecked")
	private void build(Object[] e) {
		
		if (e == null)
			return;

		start(Statements.clazz((Class<? extends Statement>) e[0]), 
				(String) e[1], (String) e[2], (String) e[3], (String) e[4]);
		
		build((Object[]) e[5]);
		end();
		
	}

}
