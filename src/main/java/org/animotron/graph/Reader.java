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

import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
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
		System.out.println("READER");
		try {
			process(position);
			out.close();
		} catch (IOException e) {
			// TODO: handle exception
		}
	}
	
	private void process(Relationship position) throws IOException {
		String typeName = position.getType().toString();
		Node eNode = position.getEndNode();
		System.out.println(position);
		
		if (RelationshipTypes.RESULT.name().equals(typeName)) {
			
			String name = typeName;
			for (Relationship r : eNode.getRelationships(Direction.INCOMING)) {
				String tmp = r.getType().toString();
				if (tmp.equals("HAVE")) {
					Relationship ref = eNode.getSingleRelationship(RelationshipTypes.REF, Direction.OUTGOING);
					eNode = ref.getEndNode();
					name = "have:"+eNode.getProperty("NAME");
					break;
				} else if (tmp.startsWith("the:")) {
					eNode = r.getEndNode();
					name = "the:"+eNode.getProperty("NAME");
					break;
				}
					
			}

			//how to find type???
			out.write(("<"+name+">").getBytes());
			
			for (Relationship r : eNode.getRelationships(Direction.OUTGOING)) {
				process(r);
			}

			out.write(("</"+name+">").getBytes());
			
		} else if (typeName.startsWith("the:")) {
			out.write(("<"+typeName+">").getBytes());
			
			for (Relationship r : eNode.getRelationships(Direction.OUTGOING)) {
				process(r);
			}

			out.write(("</"+typeName+">").getBytes());
		
		} else if (RelationshipTypes.HAVE.name().equals(typeName)) {
			String name = null;
			for (Relationship r : td_have_name.traverse(eNode).relationships()) {
				name = (String) r.getEndNode().getProperty("NAME");
			}

			out.write(("<have:"+name+">").getBytes());
			
			for (Relationship r : eNode.getRelationships(Direction.OUTGOING)) {
				process(r);
			}

			out.write(("</have:"+name+">").getBytes());
		
		} else if (RelationshipTypes.TEXT.name().equals(typeName)) {
			out.write(((String)eNode.getProperty("VALUE")).getBytes());
			
		} else {
			for (Relationship r : eNode.getRelationships(RelationshipTypes.RESULT, Direction.OUTGOING)) {
				process(r);
			}
		}
	}
	
	private static TraversalDescription td_have_name = 
		Traversal.description().
			breadthFirst().
			relationships(RelationshipTypes.REF, Direction.OUTGOING );

}
