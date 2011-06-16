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

import static org.animotron.manipulator.Executor.execute;

import java.io.IOException;
import java.util.List;

import javolution.util.FastList;

import org.animotron.graph.RelationshipTypes;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Calculator extends GraphListener {
	
	public static Calculator _ = new Calculator();
	
	private Calculator() {
		super(RelationshipTypes.CALC, Creative._);
	}

	public PipedInputObjectStream eval(PropertyContainer op) throws IOException {
		PipedInputObjectStream in = new PipedInputObjectStream();
		execute(Evaluator._.walk(op, new PipedOutputObjectStream(in)));
		return in;
	}

	public static void eval(Relationship op, PipedOutputObjectStream out) {
		execute(Evaluator._.walk(op, out));
	}
	
	public List<Relationship> evalGetResult(PropertyContainer op) throws IOException {
		PipedInputObjectStream in = new PipedInputObjectStream();
		execute(Evaluator._.walk(op, new PipedOutputObjectStream(in)));
		
		List<Relationship> result = new FastList<Relationship>();
		for (Object obj : in) {
			if (obj instanceof Relationship) {
				result.add((Relationship) obj);
			} else
				System.out.println("evalGetResult");
		}
		return result;
	}
	
	public PipedInputObjectStream prepare(PropertyContainer op) throws IOException {
		PipedInputObjectStream in = new PipedInputObjectStream();
		execute(Preparator._.walk(op, new PipedOutputObjectStream(in)));
		return in;
	}

	public void prepare(Relationship op, PipedOutputObjectStream out) {
		execute(Preparator._.walk(op, out));
	}
	
	public PipedInputObjectStream filter(Relationship op) throws IOException {
		PipedInputObjectStream in = new PipedInputObjectStream();
		execute(Filter._.walk(op, new PipedOutputObjectStream(in)));
		return in;
	}

	public void filter(Relationship op, PipedOutputObjectStream out) {
		execute(Filter._.walk(op, out));
	}
	
	@Override
    public void push(final Relationship op, PipedOutputObjectStream out) {
		System.out.println("Prepare the relationship " + op);
		try {
			prepare(op);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}

}
