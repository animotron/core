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
package org.animotron.manipulator;

import static org.animotron.graph.AnimoGraph.beginTx;
import static org.animotron.graph.AnimoGraph.finishTx;
import static org.animotron.graph.AnimoGraph.getOrCreateNode;
import static org.animotron.graph.AnimoGraph.getROOT;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import org.animotron.graph.RelationshipTypes;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class GC {
	
	private static int THREADS_NUMBER = 10;
	private static final Node GC;
	
	static {
		Transaction tx = beginTx();
		try {
			GC = getOrCreateNode(getROOT() ,RelationshipTypes.GC);
			tx.success();
		} finally {
			finishTx(tx);
		}
	}
	
	private static Executor exec = Executors.newFixedThreadPool(THREADS_NUMBER);
	
	public static void push(final Relationship op) {
		
		System.out.println("GC the relationship " + op);
		
	}
}
