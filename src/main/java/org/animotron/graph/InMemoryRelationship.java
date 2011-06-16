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
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron.graph;

import java.util.Map;

import javolution.util.FastMap;

import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class InMemoryRelationship implements Relationship {
	
	private Map<String, Object> properties = new FastMap<String, Object>();
	
	private Node sNode, eNode;
	private RelationshipType type;
	
	public InMemoryRelationship(Node sNode, Node eNode, RelationshipType type) {
		if (sNode == null) throw new IllegalArgumentException( " start node can't be NULL");
		if (eNode == null) throw new IllegalArgumentException( " end node can't be NULL");
		if (type == null) throw new IllegalArgumentException( " type can't be NULL");

		this.sNode = sNode;
		this.eNode = eNode;
		this.type = type;
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.PropertyContainer#getGraphDatabase()
	 */
	@Override
	public GraphDatabaseService getGraphDatabase() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.PropertyContainer#hasProperty(java.lang.String)
	 */
	@Override
	public boolean hasProperty(String key) {
		return properties.containsKey(key);
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.PropertyContainer#getProperty(java.lang.String)
	 */
	@Override
	public Object getProperty(String key) {
		return properties.get(key);
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.PropertyContainer#getProperty(java.lang.String, java.lang.Object)
	 */
	@Override
	public Object getProperty(String key, Object defaultValue) {
		Object value = properties.get(key);
		
		if (value == null)
			return defaultValue;
		
		return value;
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.PropertyContainer#setProperty(java.lang.String, java.lang.Object)
	 */
	@Override
	public void setProperty(String key, Object value) {
		properties.put(key, value);
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.PropertyContainer#removeProperty(java.lang.String)
	 */
	@Override
	public Object removeProperty(String key) {
		return properties.remove(key);
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.PropertyContainer#getPropertyKeys()
	 */
	@Override
	public Iterable<String> getPropertyKeys() {
		return properties.keySet();
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.PropertyContainer#getPropertyValues()
	 */
	@Override
	public Iterable<Object> getPropertyValues() {
		return properties.values();
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.Relationship#getId()
	 */
	@Override
	public long getId() {
		// TODO Auto-generated method stub
		return 0;
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.Relationship#delete()
	 */
	@Override
	public void delete() {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.Relationship#getStartNode()
	 */
	@Override
	public Node getStartNode() {
		return sNode;
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.Relationship#getEndNode()
	 */
	@Override
	public Node getEndNode() {
		return eNode;
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.Relationship#getOtherNode(org.neo4j.graphdb.Node)
	 */
	@Override
	public Node getOtherNode(Node node) {
		if (sNode.equals(node))
			return eNode;

		if (eNode.equals(node))
			return sNode;
		
		throw new IllegalArgumentException( "" + node );
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.Relationship#getNodes()
	 */
	@Override
	public Node[] getNodes() {
		return new Node[] { getStartNode(), getEndNode() };
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.Relationship#getType()
	 */
	@Override
	public RelationshipType getType() {
		return type;
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.Relationship#isType(org.neo4j.graphdb.RelationshipType)
	 */
	@Override
	public boolean isType(RelationshipType type) {
		return type.equals(type);
	}

}
