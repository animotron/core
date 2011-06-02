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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;

import org.animotron.Quanta;
import org.animotron.Statements;
import org.animotron.instruction.ml.TEXT;
import org.animotron.operator.query.ALL;
import org.animotron.operator.query.ANY;
import org.animotron.operator.relation.IS;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipExpander;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Reader implements Runnable {
	
	public static InputStream read(Relationship position) throws IOException {
		PipedInputStream in = new PipedInputStream();

		PipedOutputStream out = new PipedOutputStream(in);
		
		Thread th = new Thread(new Reader(position, out));
		th.start();
		
		return in;
	}
	
	private Relationship position;
	private OutputStream out;
	
	public Reader(Relationship position, OutputStream out) {
		this.position = position;
		this.out = out;
	}

	@Override
	public void run() {
		System.out.println("");
		System.out.println("READER");
		try {
			process(position);
			out.close();
		} catch (IOException e) {
			// TODO: handle exception
		}
	}
	
	private void subprocess(Node node) throws IOException {
		for (Relationship r : node.getRelationships(Direction.OUTGOING)) {
			process(r);
		}
	}

	private void process(Relationship position) throws IOException {
		
		Quanta st = Statements.resultRelationshipType(position.getType());
		
		String typeName = position.getType().toString();
		Node eNode = position.getEndNode();
		System.out.println(position);
		
		if (st instanceof ANY) {
			Node sNode = position.getStartNode();
			System.out.println("eNode = "+eNode);
			for (Relationship r : td_is_down.traverse(eNode).relationships()) {
				process( new InMemoryRelationship(sNode, r.getEndNode(), RelationshipTypes.RESULT) );
			}
		} else if (st instanceof ALL) {
			for (Relationship r : td_is_down.traverse(eNode).relationships()) {
				System.out.println("ALL is down = "+r.getEndNode());
			}
			
		} else if (RelationshipTypes.RESULT.name().equals(typeName)) {
			
			//how to find type???
			String name = typeName;
			for (Relationship r : eNode.getRelationships(Direction.INCOMING)) {
				String tmp = r.getType().toString();
				if (tmp.equals("HAVE")) {
					Relationship ref = eNode.getSingleRelationship(RelationshipTypes.REF, Direction.OUTGOING);

					name = "have:"+ref.getEndNode().getProperty("NAME");
					break;
				} else if (tmp.startsWith("the:")) {

					name = "the:"+r.getEndNode().getProperty("NAME");
					break;
				}
			}

			out.write(("<"+name+">").getBytes());
			
			subprocess(eNode);

			out.write(("</"+name+">").getBytes());
			
		} else if (typeName.startsWith("the:")) {
			out.write(("<"+typeName+">").getBytes());
			
			subprocess(eNode);

			out.write(("</"+typeName+">").getBytes());
		
		} else if ("HAVE".equals(typeName)) {
			String name = null;
			for (Relationship r : td_have_name.traverse(eNode).relationships()) {
				name = (String) r.getEndNode().getProperty("NAME");
			}

			out.write(("<have:"+name+">").getBytes());
			
			subprocess(eNode);

			out.write(("</have:"+name+">").getBytes());
		
		} else if (TEXT.getInstance().name().toUpperCase().equals(typeName)) {
			out.write(((String)eNode.getProperty("VALUE")).getBytes());
			subprocess(eNode);
			
		} else {
			for (Relationship r : eNode.getRelationships(Direction.OUTGOING)) {
				String type = r.getType().name();
				if (type.startsWith("RESULT") || type.startsWith("QUERY"))
					process(r);
			}
		}
	}
	
	private static TraversalDescription td_have_name = 
		Traversal.description().
			breadthFirst().
			relationships(RelationshipTypes.REF, Direction.OUTGOING );

	private static TraversalDescription td_is_down = 
		Traversal.description().
			breadthFirst().
			relationships(IS.getInstance().relationshipType(), Direction.INCOMING ).
			expand(new RelationshipExpander() {
				
				@Override
				public RelationshipExpander reversed() {
					// TODO Auto-generated method stub
					return null;
				}
				
				@Override
				public Iterable<Relationship> expand(Node node) {
					// TODO Auto-generated method stub
					return null;
				}
			});

}
