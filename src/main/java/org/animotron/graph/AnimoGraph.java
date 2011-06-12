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

import static org.neo4j.graphdb.Direction.INCOMING;
import static org.neo4j.graphdb.Direction.OUTGOING;

import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.kernel.EmbeddedGraphDatabase;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class AnimoGraph {

	public static GraphDatabaseService graphDb;
	
	protected static String STORAGE;
	
	protected static Node ROOT;
	protected static Node CACHE;

	public static Node CALC;

	protected static Node EMPTY, GC, TOP;
	protected static OrderIndex ORDER;
	
	private static final String CACHE_PREFIX = RelationshipTypes.CACHE.name().toLowerCase();
	
	public static void startDB(String folder) {
		
		STORAGE = folder;
		graphDb = new EmbeddedGraphDatabase(STORAGE);
		
		ROOT = graphDb.getReferenceNode();
		
		IndexManager INDEX = graphDb.index();
		ORDER = new OrderIndex(INDEX);
		
		Transaction tx = beginTx();
		
		try {
			GC = getOrCreateNode(ROOT, RelationshipTypes.GC);
			TOP = getOrCreateNode(ROOT, RelationshipTypes.TOP);
			CALC = getOrCreateNode(ROOT,RelationshipTypes.CALC);
			EMPTY = getOrCreateNode(ROOT,RelationshipTypes.EMPTY);
			CACHE = getOrCreateNode(ROOT, RelationshipTypes.CACHE);
			tx.success();
		} finally {
			tx.finish();
		}
		
	}
	
	public static Node getROOT() {
		return ROOT;
	}
	
	public static OrderIndex getORDER() {
		return ORDER;
	}

	public static void shutdownDB() {
		System.out.println("shotdown");
		graphDb.shutdown();
	}
	
	public static Transaction beginTx() {
		return graphDb.beginTx();
	}
	
	/**
	 * Execute operation with transaction.
	 * @param <T>
	 * 
	 * @param operation
	 * @return 
	 */
	public static <T> T execute(GraphOperation<T> operation) {
		T result = null;
		Transaction tx = beginTx();
		try {
			result = operation.execute();
			
			tx.success();
			
			return result;
		} finally {
			tx.finish();
		}
	}
	
	public static void clear (Node node){
		for (Relationship r : node.getRelationships(OUTGOING)){
			Node end = r.getEndNode();
			r.delete();
			if (!end.hasRelationship(INCOMING)) {
				GC.createRelationshipTo(end, RelationshipTypes.GARBAGE);
			}
		}
	}
	
	public static Node getNode(Node parent, RelationshipType type) {
		Relationship r = parent.getSingleRelationship(type, OUTGOING);
		return r == null ? null : r.getEndNode();
	};
	
	public static Node createNode(){
		return graphDb.createNode();
	}

	public static Node createNode(Node parent, RelationshipType type) {
		Node node = createNode();
		parent.createRelationshipTo(node, type);
		return node;
	}
	
	public static Node getOrCreateNode(Node parent, RelationshipType type) {
		Relationship r = parent.getSingleRelationship(type, OUTGOING);
		if (r != null)
			return r.getEndNode();
		Node node = createNode(parent, type);
		return node;
	}

	public static Node createCache(String hash) {
		return createCache(createNode(), hash);
	}

	public static Node createCache(Node node, String hash) {
		RelationshipType type = AnimoRelationshipType.get(hash, CACHE_PREFIX);
		CACHE.createRelationshipTo(node, type);
		return node;
	}

	public static Node getCache(String hash) {
		RelationshipType type = AnimoRelationshipType.get(hash, CACHE_PREFIX);
		return getNode(CACHE, type);
	}
	
	public static void order (Relationship r, int order) {
		ORDER.add(r, order);
		//ORDER.set(r, order);
	}
}
