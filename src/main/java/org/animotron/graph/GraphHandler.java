package org.animotron.graph;

import org.animotron.Statement;
import org.neo4j.graphdb.Node;

public interface GraphHandler {

	public abstract void start(Statement statement, String ns, String name, String value);

	public abstract void end(Statement statement, String ns, String name, String value);

	public abstract Node value(String value, byte[] bytes);

	void startDocument();

	void endDocument();

}