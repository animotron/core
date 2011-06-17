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

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class GraphListener implements Listener {
	
	private Node root;
	private RelationshipType type;
	
	public GraphListener(final RelationshipType type, Broadcaster... broadcasters) {
		this.type = type;
		for (Broadcaster b : broadcasters) {
			b.register(this);
		}
		
		root = AnimoGraph.execute(new GraphOperation<Node>() {
			@Override
			public Node execute() {
				return getOrCreateNode(getROOT(), type);
			}
		});
		
	}

	public Node getRoot() {
		return root;
	}
	
	@Override
	public final void push(Relationship op, Catcher<? extends Walker<? extends Manipulator>> catcher) throws ExceptionBuilderTerminate {
		//root.createRelationshipTo(op.getEndNode(), type);
		//TODO add pipe
		push(op, catcher, null);
	}
	
	public abstract void push(Relationship op, Catcher<? extends Walker<? extends Manipulator>> catcher, PipedOutputObjectStream out);
	
}
