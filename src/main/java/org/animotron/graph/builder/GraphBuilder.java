/*
 *  Copyright (C) 2011 The Animo Project
 *  http://animotron.org
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02129, USA.
 */
package org.animotron.graph.builder;

import org.animotron.exception.AnimoException;
import org.animotron.exception.ENotFound;
import org.animotron.graph.GraphOperation;
import org.animotron.manipulator.Manipulators;
import org.animotron.manipulator.Manipulators.Catcher;
import org.animotron.statement.Statement;
import org.animotron.statement.ml.TEXT;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.animotron.statement.relation.Relation;
import org.animotron.utils.MessageDigester;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;

import java.security.MessageDigest;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;
import java.util.StringTokenizer;

import static org.animotron.Properties.HASH;
import static org.animotron.graph.AnimoGraph.*;

/**
 * Animo graph builder, it do optimization/compression and 
 * inform listeners on store/delete events.
 * 
 * Direct graph as input from top element to bottom processing strategy.
 * 
 * Methods to use:
 * 
 * startGraph()
 * endGraph()
 * 
 * start(String prefix, String ns, String reference, String value)
 * start(Statement statement, String prefix, String ns, String reference, String value)
 * end()
 * 
 * getRelationship()
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public abstract class GraphBuilder {
	
	private Relationship the = null;
	
	private Stack<Object[]> statements;
	private Catcher catcher;

	private List<Object[]> flow;

	protected Transaction tx;

    private boolean ignoreNotFound;

    public GraphBuilder() {
        this.ignoreNotFound = true;
    }

    public GraphBuilder(boolean ignoreNotFound) {
        this.ignoreNotFound = ignoreNotFound;
    }

	public final Relationship getRelationship() {
		return the;
	}
	
	final protected void startGraph() {
		catcher = Manipulators.getCatcher();

		statements = new Stack<Object[]>();
		flow = new LinkedList<Object[]>();
		the = null;
		tx = beginTx();
	}
	
	final public boolean successful() {
		return the != null;
	}
	
    final protected void endGraph() throws AnimoException {
        endGraph(null);
    }

	final protected void endGraph(GraphOperation<?> o) throws AnimoException {

        if (!statements.empty()) {
            end();
        }

		try {
			
			int i = 0;

			for (Object[] item : flow) {
				build(item, i++);
				if (i % 100 == 0)
					System.out.println(i);

				if (i % 10000 == 0) {
					tx.success();
					finishTx(tx);
					
					tx = beginTx();
				}
			}

            the = (Relationship) flow.get(0)[3];

            if (o != null) o.execute();

			tx.success();
			finishTx(tx);
			
			catcher.push();

        } catch (AnimoException e) {
            fail(e);
            throw e;
		} catch (Exception e) {
			fail(e);
		}
			
	}

    final protected void start(Statement statement) {
        start(statement, null);
    }

    final protected void start(String value) {
        start(TEXT._, value);
    }

	final protected void start(Statement statement, String reference) {
		
        if (flow.isEmpty() && !(statement instanceof THE)) {
            start(THE._, null);
        }

		Object[] parent = null;
		
		if (!statements.empty()) {
			if (statement instanceof THE) {
				start(AN._, reference);
				end();
			} else {
				parent = statements.peek();
			}
		}

        MessageDigest md = statement.hash(reference);
		
		Object[] item = {
				statement,	// 0  statement
                reference,  // 1  name or value
				md, 		// 2  message digest
				null,	 	// 3  current relationship
				null,		// 4  current node
				parent, 	// 5  parent item
				false,		// 6  done
			};
		
		statements.push(item);
		flow.add(item);
	}
	
	final protected void end() {
		Object[] current = statements.pop();
		byte[] hash = ((MessageDigest) current[2]).digest();
		Object[] parent = (Object[]) current[5];
		if (parent != null) {
			((MessageDigest) (parent[2])).update(hash);
		}
		current[2] = hash;
	}
	
	final protected String removeWS(String value) {
		
		StringBuilder buf = new StringBuilder();
		
		if (value.length() > 0) {
			StringTokenizer tok = new StringTokenizer(value);
			while (tok.hasMoreTokens()) {
                buf.append(tok.nextToken());
				if (tok.hasMoreTokens()) buf.append(' ');
			}
		}
		
		if (buf.length() > 0)
			return buf.toString();
			
		return null;
	}
	
	//TODO: Store hash for every node as byte[]
	//TODO: Build graph via single thread in sync and async modes 
	
	private void build(Object[] item, int order) throws AnimoException {

		Object[] p =  (Object[]) item[5];
		if (p != null) {
			if ((Boolean) p[6]) {
				item[6] = true; 
				return;
			}
		}

        Relationship r;
        Statement statement = (Statement) item[0];
        if (statement instanceof THE) {
            THE the = (THE) statement;
            String hash = hash(item);
            String name = item[1] != null ? (String) item[1] : hash;
            r = the.get(name);
            if (r != null) {
                if (HASH.has(r)) {
                    String h = HASH.get(r);
                    if (h == null) {
                        catcher.creative(r);
                        HASH.set(r, hash);
                    } else if (!h.equals(hash)) {
                        catcher.renew(r);
                        HASH.set(r, hash);
                    } else {
                        item[6] = true;
                    }
                } else {
                    catcher.creative(r);
                    HASH.set(r, hash);
                }
            } else {
                r = the.create(name, hash);
                catcher.creative(r);
            }
        } else {
            Node parent = (Node) p[4];
            if (parent == null)
            	//"Internal error: parent can not be null."
                throw new AnimoException((Relationship)item[3]);

            if (!(statement instanceof Relation)) {
                String hash = hash(item);
                Node node = getCache(hash);
                if (node == null) {
                    r = build(statement, parent, item, order);
                    createCache(r.getEndNode(), hash);
                } else {
                    r = parent.createRelationshipTo(node, statement.relationshipType());
                    order(r, order);
                    item[6] = true;
                }
            } else {
                r = build(statement, parent, item, order);
            }
        }
        item[3] = r;
        item[4] = r.getEndNode();

	}
	
	private String hash (byte[] md) {
		return MessageDigester.byteArrayToHex(md); 
	}
	
	private String hash(Object[] item) {
		return hash((byte[]) item[2]);
	}
	
	private Relationship build(Statement statement, Node parent, Object[] item, int order) throws ENotFound {
		return statement.build(parent, (String) item[1], order, ignoreNotFound);
	}
	
	protected void fail(Exception e){
		e.printStackTrace(System.out);
		finishTx(tx);
        the = null;
	}
	
}
