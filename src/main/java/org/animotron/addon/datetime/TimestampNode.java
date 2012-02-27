/*
 *  Copyright (C) 2011-2012 The Animo Project
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
package org.animotron.addon.datetime;

import org.joda.time.DateTime;
import org.neo4j.graphdb.*;
import org.neo4j.graphdb.Traverser.Order;

import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.Map;

/**
 * Virtual time object.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class TimestampNode implements Node {
	
	protected static Map<Integer, Class<? extends TimestampNode>> types = 
		new HashMap<Integer, Class<? extends TimestampNode>>();
	
	public static void addType(char symbol, Class<? extends TimestampNode> clazz) {
		types.put(Integer.valueOf(symbol), clazz);
	}

	//find extends of TimeNode, current is work-around
	static {
		TimestampNode.addType(DateNode.SYMBOL, DateNode.class);
		
		TimestampNode.addType(DateNodeYear.SYMBOL, DateNodeYear.class);
		TimestampNode.addType(DateNodeMonth.SYMBOL, DateNodeMonth.class);
		
		TimestampNode.addType(TimeNode.SYMBOL, TimeNode.class);
		
		TimestampNode.addType(TimeNodeHour.SYMBOL, TimeNodeHour.class);
		TimestampNode.addType(TimeNodeMinute.SYMBOL, TimeNodeMinute.class);
		TimestampNode.addType(TimeNodeSecond.SYMBOL, TimeNodeSecond.class);

		TimestampNode.addType(DurationNode.SYMBOL, DurationNode.class);
	}
	
	protected static TimestampNode getInstance(GraphDatabaseService db, String name) throws IllegalArgumentException {
		Integer type = Integer.valueOf(name.charAt(0));
		
		Class<? extends TimestampNode> clazz = types.get(type);
		if (clazz != null)
			try {
				Constructor<? extends TimestampNode> constructor = clazz.getConstructor(GraphDatabaseService.class, String.class);
				return constructor.newInstance(db, name);
				
			} catch (IllegalArgumentException e) {
				throw e;
			} catch (Throwable t) {
				//should not happen
				throw new IllegalArgumentException(t);
			}
		
		throw new IllegalArgumentException("unknown instance '"+name+"'");
	}
	
	private GraphDatabaseService db;
	private String name;

	protected DateTime dt = null;
	
	public TimestampNode(GraphDatabaseService db, String name) {
		this.db = db;
		this.name = name;
	}

	@Override
	public GraphDatabaseService getGraphDatabase() {
		return db;
	}

	@Override
	public boolean hasProperty(String key) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Object getProperty(String key) {
		if ("NAME".equals(key))
			return name;

		throw new NotFoundException( key );
	}

	@Override
	public Object getProperty(String key, Object defaultValue) {
		throw new NotFoundException( key );
	}

	@Override
	public void setProperty(String key, Object value) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Object removeProperty(String key) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Iterable<String> getPropertyKeys() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Iterable<Object> getPropertyValues() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public long getId() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void delete() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Iterable<Relationship> getRelationships() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean hasRelationship() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Iterable<Relationship> getRelationships(RelationshipType... types) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean hasRelationship(RelationshipType... types) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Iterable<Relationship> getRelationships(Direction dir) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean hasRelationship(Direction dir) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Iterable<Relationship> getRelationships(RelationshipType type,
			Direction dir) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean hasRelationship(RelationshipType type, Direction dir) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Relationship getSingleRelationship(RelationshipType type,
			Direction dir) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Relationship createRelationshipTo(Node otherNode,
			RelationshipType type) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Traverser traverse(Order traversalOrder,
			StopEvaluator stopEvaluator,
			ReturnableEvaluator returnableEvaluator,
			RelationshipType relationshipType, Direction direction) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Traverser traverse(Order traversalOrder,
			StopEvaluator stopEvaluator,
			ReturnableEvaluator returnableEvaluator,
			RelationshipType firstRelationshipType, Direction firstDirection,
			RelationshipType secondRelationshipType, Direction secondDirection) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Traverser traverse(Order traversalOrder,
			StopEvaluator stopEvaluator,
			ReturnableEvaluator returnableEvaluator,
			Object... relationshipTypesAndDirections) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Iterable<Relationship> getRelationships(Direction arg0,
			RelationshipType... arg1) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean hasRelationship(Direction arg0, RelationshipType... arg1) {
		// TODO Auto-generated method stub
		return false;
	}

}
