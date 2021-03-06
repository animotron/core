/*
 *  Copyright (C) 2011-2013 The Animo Project
 *  http://animotron.org
 *  	
 *  This file is part of Animotron.
 *  
 *  Animotron is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as 
 *  published by the Free Software Foundation, either version 3 of 
 *  the License, or (at your option) any later version.
 *  
 *  Animotron is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *  
 *  You should have received a copy of 
 *  the GNU Affero General Public License along with Animotron.  
 *  If not, see <http://www.gnu.org/licenses/>.
 */
package org.animotron.inmemory;

import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

import java.util.HashMap;
import java.util.Map;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class InMemoryRelationship implements Relationship {
	
	private final Map<String, Object> properties = new HashMap<String, Object>();
	
	private final Node sNode, eNode;
	private final RelationshipType type;
	
	public InMemoryRelationship(final Node sNode, final Node eNode, final RelationshipType type) {
		if (sNode == null && eNode == null) throw new IllegalArgumentException( " start node can't be NULL");
		if (eNode == null && sNode == null) throw new IllegalArgumentException( " end node can't be NULL");
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
	 * @see org.neo4j.graphdb.PropertyContainer#hasProperty(java.lang.VALUE)
	 */
	@Override
	public boolean hasProperty(String key) {
		return properties.containsKey(key);
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.PropertyContainer#getProperty(java.lang.VALUE)
	 */
	@Override
	public Object getProperty(String key) {
		return properties.get(key);
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.PropertyContainer#getProperty(java.lang.VALUE, java.lang.Object)
	 */
	@Override
	public Object getProperty(String key, Object defaultValue) {
		Object value = properties.get(key);
		
		if (value == null)
			return defaultValue;
		
		return value;
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.PropertyContainer#setProperty(java.lang.VALUE, java.lang.Object)
	 */
	@Override
	public void setProperty(String key, Object value) {
		properties.put(key, value);
	}

	/* (non-Javadoc)
	 * @see org.neo4j.graphdb.PropertyContainer#removeProperty(java.lang.VALUE)
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
		return this.type.equals(type);
	}
	 
	public String toString() {
		return ""+getStartNode()+" -- ["+getType()+"] -> "+getEndNode();
	}
}