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
package org.animotron.exist.interpreter;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Iterator;

import org.animotron.exist.index.RelationshipTypes;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
class Evaluator implements Runnable {
	
	private Node node;
	private OutputStream out;
	
	public Evaluator(Node node, OutputStream out) {
		this.node = node;
		this.out = out;
	}

	@Override
	public void run() {
		
		try {
			Relationship r = null;
			
			Iterator<Relationship> it = node.getRelationships(Direction.OUTGOING).iterator();
			while (it.hasNext()) {
				
				r = it.next();
				RelationshipType type = r.getType();
				
				if (type instanceof RelationshipTypes) {
					RelationshipTypes op = (RelationshipTypes) type;
					
					switch (op) {
					case AN:
						//an:empty (return nothing)
						//an:context (process children)
						//an:self (return root)
						//an:* (reference)
						
						out.write("an ".getBytes());
						break;
	
					case ANY:
						
						out.write("any ".getBytes());
						break;
	
					case ALL:
						
						out.write("all ".getBytes());
						break;
					
					case PTRN:
						
						out.write("ptrn ".getBytes());
						break;
					
					case GET:
						
						out.write("get ".getBytes());
						break;
	
					case SELF:
						//self:instance
						//self:*
						
						out.write("self ".getBytes());
						break;
	
					case DO:
						//do:skip (return children)
						//do:xquery (perform in-line XQuery)
						//do:xslt (perform in-line XSLT)
						
						out.write("do ".getBytes());
						break;
	
					default:
						out.write("? ".getBytes());
						break;
					}
				} else {
					//others
				}
			}

			out.write("\n".getBytes());
			out.close();
		
		} catch (IOException e) {
			e.printStackTrace();
			//XXX: terminate?
		}
		
	}
}