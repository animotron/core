package org.animotron.operator;

import java.io.IOException;

import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.Relationship;

public interface Evaluable {
	
	public abstract void eval(Relationship op, PipedOutputObjectStream out,
			boolean isLast) throws IOException;
	
}