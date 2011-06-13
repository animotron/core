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

import static org.animotron.graph.AnimoGraph.beginTx;
import static org.animotron.graph.AnimoGraph.finishTx;
import static org.animotron.graph.AnimoGraph.getOrCreateNode;
import static org.animotron.graph.AnimoGraph.getROOT;

import java.io.IOException;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import org.animotron.graph.RelationshipTypes;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Calculator {
	
	private static int THREADS_NUMBER = 100;
	private static final Node CALC;
	
	static {
		Transaction tx = beginTx();
		try {
			CALC = getOrCreateNode(getROOT() ,RelationshipTypes.CALC);
			tx.success();
		} finally {
			finishTx(tx);
		}
	}
	
	private static Executor exec = Executors.newFixedThreadPool(THREADS_NUMBER);
	
	public static PipedInputObjectStream eval(Relationship op) throws IOException {
		PipedInputObjectStream in = new PipedInputObjectStream();
		exec.execute(new Evaluator(op, new PipedOutputObjectStream(in)));
		return in;
	}

	public static void eval(Relationship op, PipedOutputObjectStream out) {
		exec.execute(new Evaluator(op, out));
	}
	
	public static PipedInputObjectStream prepare(Relationship op) throws IOException {
		PipedInputObjectStream in = new PipedInputObjectStream();
		exec.execute(new Preparator(op, new PipedOutputObjectStream(in)));
		return in;
	}

	public static void prepare(Relationship op, PipedOutputObjectStream out) {
		exec.execute(new Preparator(op, out));
	}
	
	public static PipedInputObjectStream filter(Relationship op) throws IOException {
		PipedInputObjectStream in = new PipedInputObjectStream();
		exec.execute(new Filter(op, new PipedOutputObjectStream(in)));
		return in;
	}

	public static void filter(Relationship op, PipedOutputObjectStream out) {
//		exec.execute(new Filter(op, out));
		new Filter(op, out).run();
	}
	
	public static void push(Relationship op) {
		CALC.createRelationshipTo(op.getEndNode(), RelationshipTypes.CALC);
	}
	
}
