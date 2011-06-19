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

import static org.animotron.graph.AnimoGraph.getOrCreateNode;
import static org.animotron.graph.AnimoGraph.getROOT;

import java.io.IOException;

import org.animotron.graph.AnimoGraph;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractManipulator implements Manipulator {

	private Node root;
	private RelationshipType type;
	
	public AbstractManipulator(final RelationshipType type) {
		this.type = type;
		
		Transaction tx = AnimoGraph.beginTx();
		try {
			root = getOrCreateNode(getROOT(), type);
		} finally {
			AnimoGraph.finishTx(tx);
		}
		
	}

	@Override
	public Node root() {
		return root;
	}
	
	@Override
	public RelationshipType type() {
		return type;
	}

	
	@Override
	public PipedInputObjectStream execute(PropertyContainer op) throws IOException {
		return Executor.execute(this, op);
	}

	@Override
	public void execute(PropertyContainer op, PipedOutputObjectStream out) {
		Executor.execute(this, op, out);
	}
	
	@Override
	public boolean isPiped() {
		return true;
	}
	
}
