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

import org.neo4j.graphdb.*;
import org.neo4j.index.IndexService;
import org.neo4j.index.lucene.LuceneIndexService;
import org.neo4j.kernel.EmbeddedGraphDatabase;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class AnimoGraph {

	public static GraphDatabaseService graphDb;
	public static IndexService indexService;
	
	protected static Node ROOT;
	protected static Node CACHE, CALC, EMPTY, GC;
	private static final String CACHE_PREFIX = RelationshipTypes.CACHE.name().toLowerCase();
	
	public AnimoGraph(String folder) {
		graphDb = new EmbeddedGraphDatabase(folder);
		indexService = new LuceneIndexService(graphDb);

		ROOT = graphDb.getReferenceNode();
		
		Transaction tx = AnimoGraph.beginTx();
		try {
			GC = AnimoGraph.getOrCreateNode(ROOT, RelationshipTypes.GC);
			CALC = AnimoGraph.getOrCreateNode(ROOT,RelationshipTypes.CALC);
			EMPTY = AnimoGraph.getOrCreateNode(ROOT,RelationshipTypes.EMPTY);
			CACHE = AnimoGraph.getOrCreateNode(ROOT, RelationshipTypes.CACHE);
			tx.success();
		} finally {
			tx.finish();
		}
	}
	
	public static Node getROOT() {
		return ROOT;
	}
	
	public static void shutdownDB() {
		graphDb.shutdown();
	}
	
	public static Transaction beginTx() {
		return graphDb.beginTx();
	}
	
	public static void clear (Node node){
		for (Relationship r : node.getRelationships(Direction.OUTGOING)){
			Node end = r.getEndNode();
			r.delete();
			if (!end.hasRelationship(Direction.INCOMING)) {
				GC.createRelationshipTo(end, RelationshipTypes.GARBAGE);
			}
		}
	}
	
	public static Node getNode(Node parent, RelationshipType type) {
		Relationship r = parent.getSingleRelationship(type, Direction.OUTGOING);
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
		Relationship r = parent.getSingleRelationship(type, Direction.OUTGOING);
		if (r != null)
			return r.getEndNode();
		Node node = createNode(parent, type);
		return node;
	}

	public static Node createCache(String hash) {
		return createCache(AnimoGraph.createNode(), hash);
	}

	public static Node createCache(Node node, String hash) {
		RelationshipType type = AnimoRelationshipType.get(hash, CACHE_PREFIX);
		AnimoGraph.CACHE.createRelationshipTo(node, type);
		return node;
	}

	public static Node getCache(String hash) {
		RelationshipType type = AnimoRelationshipType.get(hash, CACHE_PREFIX);
		return AnimoGraph.getNode(AnimoGraph.CACHE, type);
	}

}
