/*
 *  Copyright (C) 2011 The Animo Project
 *  http://animotron.org
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 3
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more detail statement.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron.expression;

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.builder.GraphBuilder;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

import java.io.IOException;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractExpression implements Relationship {

    protected final GraphBuilder builder;

    public AbstractExpression (GraphBuilder builder) {
        this.builder = builder;
    }

    protected final void build() throws IOException {
        AnimoGraph.execute(operation());
        builder.catcher.push();
    }

    protected abstract GraphOperation<?> operation();

    @Override
	public GraphDatabaseService getGraphDatabase() {
		return builder.getRelationship().getGraphDatabase();
	}

	@Override
	public boolean hasProperty(String key) {
		return builder.getRelationship().hasProperty(key);
	}

	@Override
	public Object getProperty(String key) {
		return builder.getRelationship().getProperty(key);
	}

	@Override
	public Object getProperty(String key, Object defaultValue) {
		return builder.getRelationship().getProperty(key, defaultValue);
	}

	@Override
	public void setProperty(String key, Object value) {
		builder.getRelationship().setProperty(key, value);
	}

	@Override
	public Object removeProperty(String key) {
		return builder.getRelationship().removeProperty(key);
	}

	@Override
	public Iterable<String> getPropertyKeys() {
		return builder.getRelationship().getPropertyKeys();
	}

	@SuppressWarnings("deprecation")
	@Override
	public Iterable<Object> getPropertyValues() {
		return builder.getRelationship().getPropertyValues();
	}

	@Override
	public long getId() {
		return builder.getRelationship().getId();
	}

	@Override
	public void delete() {
		builder.getRelationship().delete();
	}

	@Override
	public Node getStartNode() {
		return builder.getRelationship().getStartNode();
	}

	@Override
	public Node getEndNode() {
		return builder.getRelationship().getEndNode();
	}

	@Override
	public Node getOtherNode(Node node) {
		return builder.getRelationship().getOtherNode(node);
	}

	@Override
	public Node[] getNodes() {
		return builder.getRelationship().getNodes();
	}

	@Override
	public RelationshipType getType() {
		return builder.getRelationship().getType();
	}

	@Override
	public boolean isType(RelationshipType type) {
		return builder.getRelationship().isType(type);
	}

	public String toString() {
		return builder.getRelationship().toString();
	}

}