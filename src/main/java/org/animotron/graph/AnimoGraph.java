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

import javolution.util.FastList;
import org.animotron.Executor;
import org.animotron.graph.index.Cache;
import org.animotron.graph.index.Order;
import org.animotron.graph.index.Result;
import org.animotron.graph.index.State;
import org.neo4j.graphdb.*;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.kernel.EmbeddedGraphDatabase;

import java.io.File;
import java.util.HashMap;
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

	private static Node ROOT;

    public static boolean startDB(String folder, Map<String, String> config) {
    	if (graphDb != null)
    		return false;
    	STORAGE = folder;
        graphDb = new EmbeddedGraphDatabase(STORAGE, config);
        initDB();
        return true;
    }

    private static boolean deleteDir(File dir) {
        if (dir.isDirectory()) {
            String[] children = dir.list();
            for (String aChildren : children) {
                boolean success = deleteDir(new File(dir, aChildren));
                if (!success) {
                    return false;
                }
            }
        }
        return dir.delete();
    }

    public static boolean cleanDB() {
        if (graphDb != null) {
            shutdownDB();
            return deleteDir(new File(STORAGE));
        }
        return false;
    }

    public static boolean cleanDB(String file) {
        if (file != null) {
            File f = new File(file);
            if (STORAGE != null) {
                if (f.getAbsoluteFile().equals(new File(STORAGE).getAbsoluteFile())) {
                    shutdownDB();
                }
            }
            return deleteDir(f);
        }
        return false;
    }


    public static boolean startDB(String folder) {
    	return startDB(folder, new HashMap<String, String>());
    }

    public static void initDB() {
        ROOT = graphDb.getReferenceNode();
        IndexManager INDEX = graphDb.index();
        Cache.init(INDEX);
        Order.init(INDEX);
        State.init(INDEX);
        Result.init(INDEX);
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

	public static void shutdownDB() {
        if (graphDb == null) {
            return;
        }
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
        graphDb = null;
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
		} catch (Exception e) {
			e.printStackTrace();
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
