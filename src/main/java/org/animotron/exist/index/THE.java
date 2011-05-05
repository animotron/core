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
package org.animotron.exist.index;

import java.util.Iterator;

import org.exist.dom.DocumentAtExist;
import org.exist.dom.ElementAtExist;
import org.exist.numbering.NodeId;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.traversal.Evaluators;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class THE extends AnimoNode implements Iterable<AnimoNode> {
	
	public THE(final Node gNode) {
		super(gNode);
	}
	
	public THE(final Node gNode, final ElementAtExist node) {
		super(gNode, node.getDocumentAtExist(), node.getNodeId());
	}
	

	public THE(final Node gNode, final DocumentAtExist doc, final NodeId nodeId) {
		super(gNode, doc, nodeId);
	}

	public void addIsRelationship(final THE is) {
		final GraphDatabaseService graphDb = graphNode.getGraphDatabase();
		final Transaction tx = graphDb.beginTx();
		try {
			graphNode.createRelationshipTo( is.graphNode, RelationshipTypes.IS );

			tx.success();
		} finally {
			tx.finish();
		}
	}

	@Override
	public Iterator<AnimoNode> iterator() {
		return new ProcessingFlowIterator();
	}
	
	class ProcessingFlowIterator implements Iterator<AnimoNode> {
		
		Iterator<Node> it;
		
		public ProcessingFlowIterator() {
			TraversalDescription td = Traversal.description().
			breadthFirst().
			relationships(RelationshipTypes.PROCESSING_FLOW_ELEMENT ).
			evaluator(Evaluators.excludeStartPosition());
		
			it = td.traverse( graphNode ).nodes().iterator();
		}

		@Override
		public boolean hasNext() {
			return it.hasNext();
		}

		@Override
		public AnimoNode next() {
			Node node = it.next();
			//short type = (Short) node.getProperty(KEY_NODE_TYPE);
			//if (type == 1) {
			//	return new THE(node);
			//}else if (type == 11) {
			//	return new HAVE(node);
			//}
			
			return new AnimoNode(node);
		}

		@Override
		public void remove() {
			throw new RuntimeException("Read only iterator.");
		}
		
	}
}
