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

import javolution.util.FastMap;
import org.animotron.Executor;
import org.animotron.statement.operator.THE;
import org.neo4j.graphdb.*;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.graphdb.index.RelationshipIndex;
import org.neo4j.kernel.EmbeddedGraphDatabase;

import java.io.IOException;
import java.util.Map.Entry;

import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class AnimoGraph {

	private static GraphDatabaseService graphDb;
	
	private static String STORAGE;
	
	private static Node ROOT, TOP; //EMPTY
    private static CacheIndex CACHE;
    private static OrderIndex ORDER;
	public static RelationshipIndex RESULT_INDEX;
	private static String RESULT = "RESULT";
	
	private static final String CACHE_PREFIX = RelationshipTypes.CACHE.name().toLowerCase();
	
    public static void startDB(String folder) {
        STORAGE = folder;
        graphDb = new EmbeddedGraphDatabase(STORAGE);
        initDB();
    }
	
    public static void initDB() {
        ROOT = graphDb.getReferenceNode();
        IndexManager INDEX = graphDb.index();
        CACHE = new CacheIndex(INDEX);
        ORDER = new OrderIndex(INDEX);

        RESULT_INDEX = INDEX.forRelationships(RESULT);

        execute(
            new GraphOperation<Void> () {
                @Override
                public Void execute() {
                    TOP = getOrCreateNode(ROOT, RelationshipTypes.TOP);
                    //EMPTY = getOrCreateNode(ROOT,RelationshipTypes.EMPTY);
                    return null;
                }
            }
        );
    }

	public static GraphDatabaseService getDb() {
		return graphDb;
	}
	
	public static String getStorage() {
		return STORAGE;
	}
	
	public static Node getROOT() {
		return ROOT;
	}
	
	public static Node getTOP() {
		return TOP;
	}
	
	public static OrderIndex getORDER() {
		return ORDER;
	}

	public static void shutdownDB() {
		System.out.println("shotdown");
		
		Executor.shutdown();

		while (!activeTx.isEmpty()) {
		
			System.out.println("Active transactions "+countTx);
			if (countTx > 0) {
				for (Entry<Transaction, Exception> e : activeTx.entrySet()) {
					e.getValue().printStackTrace();
				}
			}
			
			try { Thread.sleep(1000); } catch (InterruptedException e) {}
		}
		graphDb.shutdown();
		
		THE._.beforeShutdown();
	}
	
	private static FastMap<Transaction, Exception> activeTx = new FastMap<Transaction, Exception>();
	
	private static int countTx = 0;
	
	private static synchronized void inc() {
		countTx++;
	}
	
	private static synchronized void dec() {
		countTx--;
	}
	
	public static Transaction beginTx() {
		Transaction tx = graphDb.beginTx();
		activeTx.put(tx, new IOException());
		inc();
		return tx;
	}
	
	public static void finishTx(Transaction tx) {
		tx.finish();
		activeTx.remove(tx);
		dec();
	}
	
	public static boolean isTransactionActive(Transaction tx) {
		return activeTx.containsKey(tx);
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
        } finally {
			finishTx(tx);
            return result;
		}
	}
	
	public static Node getNode(Node parent, RelationshipType type) {
		Relationship r = parent.getSingleRelationship(type, OUTGOING);
		return r == null ? null : r.getEndNode();
	}
	
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

	public static void createCache(Node node, byte[] hash) {
		CACHE.add(node, hash);
	}

	public static Node getCache(byte[] hash) {
        return CACHE.get(hash);
	}
	
    public static void order (Relationship r, int order) {
        ORDER.add(r, order);
    }
	
    public static void unorder (Relationship r) {
        ORDER.remove(r);
    }

	public static void result (Relationship r, long id) {
		RESULT_INDEX.add(r, RESULT, id);
	}

	public static IndexHits<Relationship> getResult(Relationship ref, Node node) {
		return RESULT_INDEX.get(RESULT, ref.getId(), node, null);
		//return RESULT_INDEX.query(RESULT, ref.getId(), node, null);
	}
}
