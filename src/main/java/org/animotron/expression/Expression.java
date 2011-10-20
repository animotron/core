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

import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class Expression implements Relationship {

    protected abstract Relationship relationship();

    public final static Relationship[] __(Expression... e) {
        Relationship[] a = new Relationship[e.length];
        for (int i = 0; i < e.length; i++) {
            a[i] = __(e[i]);
        }
        return a;
    }

    public final static Relationship __(Expression e) {
        return e.relationship();
    }

    @Override
	public GraphDatabaseService getGraphDatabase() {
		return relationship().getGraphDatabase();
	}

	@Override
	public boolean hasProperty(String key) {
		return relationship().hasProperty(key);
	}

	@Override
	public Object getProperty(String key) {
		return relationship().getProperty(key);
	}

	@Override
	public Object getProperty(String key, Object defaultValue) {
		return relationship().getProperty(key, defaultValue);
	}

	@Override
	public void setProperty(String key, Object value) {
		relationship().setProperty(key, value);
	}

	@Override
	public Object removeProperty(String key) {
		return relationship().removeProperty(key);
	}

	@Override
	public Iterable<String> getPropertyKeys() {
		return relationship().getPropertyKeys();
	}

	@SuppressWarnings("deprecation")
	@Override
	public Iterable<Object> getPropertyValues() {
		return relationship().getPropertyValues();
	}

	@Override
	public long getId() {
		return relationship().getId();
	}

	@Override
	public void delete() {
		relationship().delete();
	}

	@Override
	public Node getStartNode() {
		return relationship().getStartNode();
	}

	@Override
	public Node getEndNode() {
		return relationship().getEndNode();
	}

	@Override
	public Node getOtherNode(Node node) {
		return relationship().getOtherNode(node);
	}

	@Override
	public Node[] getNodes() {
		return relationship().getNodes();
	}

	@Override
	public RelationshipType getType() {
		return relationship().getType();
	}

	@Override
	public boolean isType(RelationshipType type) {
		return relationship().isType(type);
	}

	public String toString() {
		return relationship().toString();
	}

}