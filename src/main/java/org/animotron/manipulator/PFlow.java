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

import static org.animotron.graph.RelationshipTypes.REF;
import static org.neo4j.graphdb.Direction.OUTGOING;

import javolution.util.FastList;
import org.animotron.operator.AN;
import org.animotron.operator.query.ALL;
import org.animotron.operator.query.ANY;
import org.animotron.operator.relation.IS;
import org.animotron.operator.relation.USE;
import org.jetlang.channels.Channel;
import org.jetlang.channels.MemoryChannel;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.Evaluator;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

import java.util.Iterator;
import java.util.List;

import static org.neo4j.graphdb.traversal.Evaluation.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class PFlow {
	
	private final Manipulator m;

	public final Channel<Relationship> answer = new MemoryChannel<Relationship>();
	public final Channel<PFlow> question = new MemoryChannel<PFlow>();
	public final Channel<Void> stop = new MemoryChannel<Void>();
	
	protected PFlow parent = null;
	private Relationship op = null;
	private Node opNode = null;
	
	private List<Relationship> path = new FastList<Relationship>();
	
	private PFlow(Manipulator m) {
		this.m = m;
	};
	
	public PFlow(Manipulator m, Relationship start_op, Relationship op) {
		parent = new PFlow(m);
		this.m = m;
		this.op = op;
		
		path.add(start_op);
	}

	public PFlow(Manipulator m, Relationship start_op, Node opNode) {
		parent = new PFlow(m);
		this.m = m;
		this.opNode = opNode;

		path.add(start_op);
	}

	public PFlow(PFlow parent, Relationship op) {
		this.parent = parent;
		this.m = parent.m;
		
		//XXX: maybe, clone faster?
		path.addAll(parent.path);
		
		this.op = op;
	}

	public PFlow(PFlow parent, Node opNode) {
		this.parent = parent;
		this.m = parent.m;
		
		//XXX: maybe, clone faster?
		path.addAll(parent.path);

		this.opNode = opNode;
	}
	
	public PFlow getParent() {
		return parent;
	}

	public Relationship getOP() {
		return op;
	}

	public Relationship getStartOP() {
		return path.get(0);
	}
	
	public Node getStartNode() {
		return path.get(0).getEndNode();
	}

	public Node getOPNode() {
		if (opNode != null)
			return opNode;
		
		return op.getEndNode();
	}

	protected void setOPNode(Node opNode) {
		this.opNode = opNode;
		this.op = null;
	}

	public void sendAnswer(Relationship r) {
		if (parent == null) {
			System.out.println("WORNG - no parent");
			//System.out.println("send answer to "+answer+" (this = "+this+")");
			answer.publish(r);
		} else {
			//System.out.println("send answer to "+parent.answer+" (parent = "+parent+")");
			parent.answer.publish(r);
		}
	}

	public void done() {
		if (parent == null) answer.publish(null);
		else parent.answer.publish(null);
	}

	public Manipulator getManipulator() {
		return m;
	}
	
	public Path getFlowPath() {
		return td_flow.traverse(getOPNode()).iterator().next();
	}
	
	public Iterable<Relationship> stack() {
		return new PFlowStack();
	}
	
	private class PFlowStack implements Iterator<Relationship>, Iterable<Relationship> {
		
		private Iterator<Relationship> it = getFlowPath().relationships().iterator();
		private Relationship pos = step();
		
		private Relationship step() {
			while (it.hasNext()) {
				Relationship r = it.next();
				if (AN._.relationshipType().name().equals(r.getType().name())){
					return r;
				}
			}
			return null;
		}
		
		@Override
		public boolean hasNext() {
			return pos != null;
		}

		@Override
		public Relationship next() {
			Relationship res = pos;
			pos = step();
			return res;
		}

		@Override
		public void remove() {
			// TODO Auto-generated method stub
		}

		@Override
		public Iterator<Relationship> iterator() {
			return this;
		}
		
	}

	public Iterable<Relationship> getStackContext(Relationship r, boolean goDown) {
//		return td_context.traverse(node).relationships();
		
		Node node = r.getEndNode();
		
		if (goDown && r.getType().name().equals(AN._.relationshipType().name())) {
			node = node.getSingleRelationship(REF, OUTGOING).getEndNode();
		}
		
		return node.getRelationships(AN._.relationshipType(), OUTGOING);
	}


	public void addContextPoint(Relationship r) {
		path.add(r);
	}

	public Relationship getLastContext() {
		return path.get(path.size()-1);
	}

//    private TraversalDescription td_context =
//            Traversal.description().depthFirst().
//            uniqueness(Uniqueness.RELATIONSHIP_PATH).
//            evaluator(new Evaluator(){
//                @Override
//                public Evaluation evaluate(Path path) {
//                    System.out.println("path = "+path);
//                    if (path.length() > 0) {
//                        Relationship r = path.lastRelationship();
//                        if (r.getStartNode().equals(path.endNode())) {
//
//                            if (r.isType(AN._.relationshipType()))
//                                return INCLUDE_AND_PRUNE;
//                            else if (r.isType(RelationshipTypes.REF)) {
//
//                                boolean found = false;
//
//                                //TODO: check for REF after AN
//                                path.iterator();
//
//                                if (found)
//                                    return INCLUDE_AND_PRUNE;
//                            }
//
//                            return EXCLUDE_AND_CONTINUE;
//                        }
//                        return EXCLUDE_AND_PRUNE;
//                    }
//                    return EXCLUDE_AND_CONTINUE;
//                }
//            });

    private TraversalDescription td_context =
            Traversal.description().depthFirst().
            uniqueness(Uniqueness.RELATIONSHIP_PATH).
            evaluator(new Evaluator(){
                @Override
                public Evaluation evaluate(Path path) {
                    System.out.println("path = "+path);
                    if (path.length() > 0) {
                        Relationship r = path.lastRelationship();
                        if (r.isType(REF)) {
                            return EXCLUDE_AND_PRUNE;
                        }
                        if (r.isType(IS._.relationshipType())) {
                            return EXCLUDE_AND_PRUNE;
                        }
                        if (r.isType(USE._.relationshipType())) {
                            return EXCLUDE_AND_PRUNE;
                        }
                        if (r.isType(AN._.relationshipType())) {
                            return INCLUDE_AND_PRUNE;
                        }
                        if (r.isType(ALL._.relationshipType())) {
                            return INCLUDE_AND_PRUNE;
                        }
                        if (r.isType(ANY._.relationshipType())) {
                            return INCLUDE_AND_PRUNE;
                        }
                        return EXCLUDE_AND_CONTINUE;
                    }
                    return Evaluation.EXCLUDE_AND_CONTINUE;
                }
            });

	private TraversalDescription td_flow =
			Traversal.description().depthFirst().
			uniqueness(Uniqueness.RELATIONSHIP_PATH).
			evaluator(new Evaluator(){
				@Override
				public Evaluation evaluate(Path path) {
					if (path.length() > 0) {
						Relationship r = path.lastRelationship(); 
						if (r.getStartNode().equals(path.endNode())) {
							if (r.equals(getStartOP())) {
								return INCLUDE_AND_PRUNE;
							} 
							return EXCLUDE_AND_CONTINUE;	
						} 
						return EXCLUDE_AND_PRUNE;
					}
					return EXCLUDE_AND_CONTINUE;
				}
			});
}