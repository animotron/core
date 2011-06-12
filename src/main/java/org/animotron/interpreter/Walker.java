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
package org.animotron.interpreter;

import static org.neo4j.graphdb.Direction.OUTGOING;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Iterator;

import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
class Walker implements Runnable {
	
	private Relationship op;
	private PipedOutputObjectStream out;
	
	private Class<? extends Statement> clazz;
	private Method method;
	
	public Walker(Class<? extends Statement> clazz, Method method, Relationship op, PipedOutputObjectStream out) {
		this.clazz = clazz;
		this.method = method;

		this.op = op;
		this.out = out;
	}

	@Override
	public void run() {
		try {
			go(op, out);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	protected void go(Relationship op, PipedOutputObjectStream ot) throws IOException {
		
		System.out.println("Walk op = "+op);
		
		GraphDatabaseService graphdb = op.getGraphDatabase();
		Transaction tx = graphdb.beginTx();
		try {
			Relationship r = null;
			
			Node node = op.getEndNode();
			System.out.println("Walk node = "+node);
			Iterator<Relationship> it = node.getRelationships(OUTGOING).iterator();
			while (it.hasNext()) {
				
				r = it.next();
				RelationshipType type = r.getType();
				
				System.out.println(type.name());
				
				Statement s = Statements.relationshipType(type);

				if (s == null)
					;//???
				else if (implementsInterface(s.getClass(), clazz)) {

					PipedInputObjectStream in = new PipedInputObjectStream();

					try {
						method.invoke(s, r, new PipedOutputObjectStream(in), isLast(it));
					
						//XXX: what to do on exception???
					} catch (IllegalArgumentException e) {
						e.printStackTrace();
					} catch (IllegalAccessException e) {
						e.printStackTrace();
					} catch (InvocationTargetException e) {
						e.printStackTrace();
					}
					//expr.eval(r, new PipedOutputObjectStream(in), isLast(it));
					
					for (Object n : in) {
						ot.write(n);
					} 
					
				} else {
					System.out.println("Not evaled "+r);
					ot.write(r);
				}
			}
			tx.success();
		
		} catch (IOException e) {
			e.printStackTrace();
			ot.write(e);
		} finally {
			tx.finish();
		}

		ot.close();
	}
		
	protected boolean isLast(Iterator<?> it) {
		return !it.hasNext();
	}
	
    private static Boolean implementsInterface(Class<?> object, Class<?> intrfc){
        for (Class<?> c : object.getInterfaces()) {
            if (c.equals(intrfc)) {
                return true;
            }
        }
        return false;
    }

}