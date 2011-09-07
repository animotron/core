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
package org.animotron;

import org.animotron.graph.builder.GraphBuilder;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractExpression extends GraphBuilder implements Relationship {

    public AbstractExpression (boolean ignoreNotFound) {
        super(ignoreNotFound);
    }

    public AbstractExpression() {
        super();
    }

    @Override
	public GraphDatabaseService getGraphDatabase() {
		return getRelationship().getGraphDatabase();
	}

	@Override
	public boolean hasProperty(String key) {
		return getRelationship().hasProperty(key);
	}

	@Override
	public Object getProperty(String key) {
		return getRelationship().getProperty(key);
	}

	@Override
	public Object getProperty(String key, Object defaultValue) {
		return getRelationship().getProperty(key, defaultValue);
	}

	@Override
	public void setProperty(String key, Object value) {
		getRelationship().setProperty(key, value);
	}

	@Override
	public Object removeProperty(String key) {
		return getRelationship().removeProperty(key);
	}

	@Override
	public Iterable<String> getPropertyKeys() {
		return getRelationship().getPropertyKeys();
	}

	@SuppressWarnings("deprecation")
	@Override
	public Iterable<Object> getPropertyValues() {
		return getRelationship().getPropertyValues();
	}

	@Override
	public long getId() {
		return getRelationship().getId();
	}

	@Override
	public void delete() {
		getRelationship().delete();
	}

	@Override
	public Node getStartNode() {
		return getRelationship().getStartNode();
	}

	@Override
	public Node getEndNode() {
		return getRelationship().getEndNode();
	}

	@Override
	public Node getOtherNode(Node node) {
		return getRelationship().getOtherNode(node);
	}

	@Override
	public Node[] getNodes() {
		return getRelationship().getNodes();
	}

	@Override
	public RelationshipType getType() {
		return getRelationship().getType();
	}

	@Override
	public boolean isType(RelationshipType type) {
		return getRelationship().isType(type);
	}

	public String toString() {
		return getRelationship().toString();
	}
}