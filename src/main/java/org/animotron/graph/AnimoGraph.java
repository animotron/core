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

import javolution.util.FastList;
import org.animotron.Executor;
import org.animotron.graph.index.Cache;
import org.animotron.graph.index.Order;
import org.animotron.graph.index.Result;
import org.animotron.statement.operator.THE;
import org.neo4j.graphdb.*;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.kernel.EmbeddedGraphDatabase;

import java.util.List;
import java.util.Map;

import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class AnimoGraph {

	private static GraphDatabaseService graphDb = null;

	private static String STORAGE;

	private static Node ROOT, START, END, TOP;

    public static void startDB(String folder, Map<String, String> config) {
    	if (graphDb != null)
    		return; //log?
    	
    	STORAGE = folder;
        graphDb = new EmbeddedGraphDatabase(STORAGE, config);
        initDB();
    }

    public static void startDB(String folder) {
        STORAGE = folder;
        graphDb = new EmbeddedGraphDatabase(STORAGE);
        initDB();
    }

    public static void initDB() {
        ROOT = graphDb.getReferenceNode();
        IndexManager INDEX = graphDb.index();
        Cache.init(INDEX);
        Order.init(INDEX);
        Result.init(INDEX);
        execute(
            new GraphOperation<Void> () {
                @Override
                public Void execute() {
                    START = getOrCreateNode(ROOT,RelationshipTypes.START);
                    END = getOrCreateNode(ROOT,RelationshipTypes.END);
                    TOP = getOrCreateNode(ROOT, RelationshipTypes.TOP);
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

    public static Node getSTART() {
        return START;
    }

    public static Node getEND() {
        return END;
    }

    public static Node getTOP() {
        return TOP;
    }

	public static void shutdownDB() {
		System.out.println("shotdown");
		Executor.shutdown();
		while (!activeTx.isEmpty()) {
			System.out.println("Active transactions "+activeTx.size());
//			if (countTx > 0) {
//				for (Entry<Transaction, Exception> e : activeTx.entrySet()) {
//					e.getValue().printStackTrace();
//				}
//			}
			try { Thread.sleep(1000); } catch (InterruptedException e) {}
		}
		graphDb.shutdown();

		THE._.beforeShutdown();
	}

	private static List<Transaction> activeTx = new FastList<Transaction>();

	public static Transaction beginTx() {
		Transaction tx = graphDb.beginTx();
		activeTx.add(tx);
		return tx;
	}

	public static void finishTx(Transaction tx) {
		tx.finish();
		activeTx.remove(tx);
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

    public static void copyProperties(PropertyContainer src, PropertyContainer dst) {
        for (String name : src.getPropertyKeys()) {
            dst.setProperty(name, src.getProperty(name));
        }
    }

    public static Relationship copy(Node start, Relationship r) {
        Relationship c = start.createRelationshipTo(r.getEndNode(), r.getType());
        copyProperties(r, c);
        return c;
    }

    public static Relationship copy(Relationship r, Node end) {
        Relationship c = r.getStartNode().createRelationshipTo(end, r.getType());
        copyProperties(r, c);
        return c;
    }

}
