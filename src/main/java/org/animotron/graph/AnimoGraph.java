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
package org.animotron.graph;

import org.animotron.Executor;
import org.animotron.graph.index.Cache;
import org.animotron.graph.index.Order;
import org.animotron.graph.index.Result;
import org.animotron.graph.index.State;
import org.animotron.statement.operator.DEF;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.*;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.kernel.EmbeddedGraphDatabase;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class AnimoGraph {

    private final static String BIN_STORAGE = "binary";
    private final static String TMP_STORAGE = "tmp";

	private static GraphDatabaseService graphDb = null;

	private static String STORAGE;

    private static List<Transaction> activeTx;
    private static Map<Transaction, Throwable> debugActiveTx;

	private static Node ROOT;
    private static File BIN;
    private static File TMP;

    public static boolean startDB(String folder, Map<String, String> config) {
        if (graphDb != null) {
            return false;
        }

        System.gc();

        activeTx = Collections.synchronizedList(new LinkedList<Transaction>());
        debugActiveTx = new ConcurrentHashMap<Transaction, Throwable>();
    	STORAGE = folder;

    	Executor.init();

        graphDb = new EmbeddedGraphDatabase(STORAGE, config);
        BIN = new File(STORAGE, BIN_STORAGE); BIN.mkdir();
        TMP = new File(STORAGE, TMP_STORAGE); TMP.mkdir();
        initDB();
        
//        final Thread mainThread = Thread.currentThread();
        Runtime.getRuntime().addShutdownHook(new Thread() {
            public void run() {
                shutdownDB();
//                mainThread.join();
            }
        });

        return true;
    }

    public static boolean startDB(String folder) {
    	return startDB(folder, new HashMap<String, String>());
    }

    public static void initDB() {
        ROOT = graphDb.getReferenceNode();
        IndexManager index = graphDb.index();
        Cache.RELATIONSHIP.init(index);
        Cache.NODE.init(index);
        Result._.init(index);
        Order._.init(index);
        State._.init(index);
        DEF._.init(index);
        VALUE._.init(index);
    }

	public static GraphDatabaseService getDb() {
		return graphDb;
	}

	public static String getStorage() {
		return STORAGE;
	}

    public static File binStorage(){
        return BIN;
    }

    public static File tmpStorage(){
        return TMP;
    }

	public static Node getROOT() {
		return ROOT;
	}

	public static void shutdownDB() {
        if (graphDb == null) {
            return;
        }
		System.out.println("shotdown");
		Executor.shutdown();
		while (!activeTx.isEmpty()) {
			System.out.println("Active transactions "+activeTx.size());

			for (Map.Entry<Transaction, Throwable> e : debugActiveTx.entrySet()) {
				if (e != null) {
					System.out.println(e.getKey());
					e.getValue().printStackTrace();
				} else {
					System.out.println("NULL @ debugActiveTx");
				}
			}

			try { Thread.sleep(1000); } catch (InterruptedException e) {}
		}
		graphDb.shutdown();
        graphDb = null;
	}

	public static Transaction beginTx() {
		Transaction tx = graphDb.beginTx();
		activeTx.add(tx);
		debugActiveTx.put(tx, new IOException());
		return tx;
	}

	public static void finishTx(final Transaction tx) {
		if (tx == null) {
			System.out.println("tx == NULL");
			return;
		}
		try {
			tx.finish();
		} finally {
			activeTx.remove(tx);
			debugActiveTx.remove(tx);
		}
	}

//	public static boolean isTransactionActive(Transaction tx) {
//		return activeTx.containsKey(tx);
//	}

	/**
	 * Execute operation with transaction.
	 * @param <T>
	 *
	 * @param operation
	 * @return
	 */
	public static <T> T execute(GraphOperation<T> operation) throws Throwable {
		T result = null;
		Transaction tx = beginTx();
		try {
			result = operation.execute();
			tx.success();
		} catch (Throwable t) {
			throw t;
        } finally {
			finishTx(tx);
		}
        return result;
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

    public static void copyProperties(PropertyContainer src, PropertyContainer dst) {
        for (String name : src.getPropertyKeys()) {
            dst.setProperty(name, src.getProperty(name));
        }
    }

    public static Relationship copy(Node start, Relationship r) {
        if (r.getStartNode().equals(start)) {
            return r;
        }
        Relationship c = start.createRelationshipTo(r.getEndNode(), r.getType());
        copyProperties(r, c);
        return c;
    }

    public static Relationship copy(Relationship r, Node end) {
        if (r.getEndNode().equals(end)) {
            return r;
        }
        Relationship c = r.getStartNode().createRelationshipTo(end, r.getType());
        copyProperties(r, c);
        return c;
    }

}
