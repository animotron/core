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
package org.animotron.operator.query;

import static org.neo4j.graphdb.Direction.OUTGOING;
import static org.neo4j.graphdb.traversal.Evaluation.EXCLUDE_AND_CONTINUE;
import static org.neo4j.graphdb.traversal.Evaluation.EXCLUDE_AND_PRUNE;
import static org.neo4j.graphdb.traversal.Evaluation.INCLUDE_AND_PRUNE;

import org.animotron.Executor;
import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.RelationshipTypes;
import org.animotron.io.PipedInput;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.operator.AbstractOperator;
import org.animotron.operator.Cachable;
import org.animotron.operator.Evaluable;
import org.animotron.operator.IC;
import org.animotron.operator.Query;
import org.animotron.operator.Utils;
import org.animotron.operator.relation.HAVE;
import org.animotron.operator.relation.IS;
import org.jetlang.channels.Subscribable;
import org.jetlang.core.DisposingExecutor;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

/**
 * Query operator 'Get'. Return 'have' relations on provided context.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class GET extends AbstractOperator implements Evaluable, Query, Cachable {

	public static final GET _ = new GET();
	
	private GET() { super("get", "animo/query/extract"); }

	private static TraversalDescription td_eval = 
		Traversal.description().
			breadthFirst().
			relationships(HAVE._.relationshipType(), OUTGOING);
			//.evaluator(Evaluators.excludeStartPosition());

	private static TraversalDescription td_eval_ic = 
		Traversal.description().
			breadthFirst().
			relationships(IS._.relationshipType(), OUTGOING).
			relationships(IC._.relationshipType(), OUTGOING);

	public OnQuestion onCalcQuestion() {
		return question;
	}

	private OnQuestion question = new OnQuestion() {

		@Override
		public void onMessage(final PFlow pf) {
			final Relationship op = pf.getOP();
			
			final Node node = op.getEndNode();
			
			System.out.println("GET '"+name(op)+"' THREAD "+Thread.currentThread());

			//check, maybe, result was already calculated
			if (!Utils.results(node, pf)) {
				//no pre-calculated result, calculate it
				
				final String name = name(op);
				
				Subscribable<Relationship> onContext = new Subscribable<Relationship>() {
					@Override
					public void onMessage(Relationship context) {
						System.out.println("GET message ["+name+"] context "+context);
						
						if (context == null) {
							pf.sendAnswer(null);
							return;
						}
						Relationship res = getByTraversal(op, context, name); //get(context.getEndNode(), name);
						if (res != null) {
							pf.sendAnswer(res);
							return;
						}

						PipedInput in;
						try {
							in = Evaluator._.execute(pf.getStartOP(), context.getEndNode());
						
							for (Object n : in) {
								res = get(((Relationship)n).getEndNode(), name);
								
								if (res != null)
									pf.sendAnswer(createResult(node, res));
							}
						} catch (Exception e) {
							//XXX: what to do?
							e.printStackTrace();
						}
					}

					@Override
					public DisposingExecutor getQueue() {
						return Executor.getFiber();
					}
				};
				pf.answer.subscribe(onContext);
				
				if (haveContext(pf)) {
					pf.addContextPoint(op);
					super.onMessage(pf);
				} else {
					System.out.println("P-FLOW is context for GET!");
					Relationship res = null;
					boolean answered = false;
					for (Relationship stack : pf.stack()) {
						System.out.println("stack "+stack);

						res = get(stack.getEndNode(), name);
						if (res != null) {
							pf.sendAnswer(createResultInMemory(node, res));
							answered = true;
						} else {
							answered = goDownStack(pf, answered, name, node, stack);
						}
						if (answered) break;
					}
				}
			}
			pf.done();
		}
	};
	
	private boolean goDownStack(PFlow pf, boolean answered, String name, Node node, Relationship pos) {
		
		Relationship res = null;
		
		//go by REF first
		System.out.println("go by REF");
		for (Relationship context : pf.getStackContext(pos, true)) {
			System.out.println(context);
			res = get(context.getEndNode(), name);
			if (res != null) {
				pf.sendAnswer(createResultInMemory(node, res));
				answered = true;
			}
		}
		if (!answered) {
			for (Relationship context : pf.getStackContext(pos, true)) {
				answered = answered || goDownStack(pf, answered, name, node, context);
			}
		}
		
		if (!answered) {
			//go by AN (references)
			System.out.println("go by AN");
			for (Relationship context : pf.getStackContext(pos, false)) {
				System.out.println(context);
				res = get(context.getEndNode(), name);
				if (res != null) {
					pf.sendAnswer(createResultInMemory(node, res));
					answered = true;
				}
			}
			if (!answered) {
				for (Relationship context : pf.getStackContext(pos, false)) {
					answered = answered || goDownStack(pf, answered, name, node, context);
				}
			}
		}

		
		return answered;
	}

	public Relationship get(Node context, final String name) {
		
		System.out.println("GET get context = "+context);

		//search for local 'HAVE'
		Relationship have = getByHave(context, name);
		if (have != null) return have;
		
		Relationship instance = context.getSingleRelationship(RelationshipTypes.REF, OUTGOING);
		if (instance != null) {
			//change context to the-instance by REF
			context = instance.getEndNode();
			
			//search for have
			have = getByHave(context, name);
			if (have != null) return have;
		}
		
		Relationship prevTHE = null;
		
		//search 'IC' by 'IS' topology
		for (Relationship tdR : td_eval_ic.traverse(context).relationships()) {
			
			Statement st = Statements.relationshipType( tdR.getType() );
			if (st instanceof IS) {
				System.out.println("GET IC -> IS "+tdR);
				if (prevTHE != null) {
					//search for have
					have = getByHave(prevTHE.getEndNode(), name);
					if (have != null) return have;
				}
				prevTHE = tdR;
				
			} else if (st instanceof IC) {
				System.out.print("GET IC -> "+tdR);
				
				if (name.equals(name(tdR))) {
					System.out.println(" MATCH");
					
					//store
					final Node sNode = context;
					final Relationship r = tdR;

					return AnimoGraph.execute(new GraphOperation<Relationship>() {
						@Override
						public Relationship execute() {
							Relationship res = sNode.createRelationshipTo(r.getEndNode(), HAVE._.relationshipType());
							//RID.set(res, r.getId());
							return res;
						}
					});
					
					//in-memory
					//Relationship res = new InMemoryRelationship(context, tdR.getEndNode(), HAVE._.relationshipType());
					//RID.set(res, tdR.getId());
					//return res;
					
					//as it
					//return tdR;
				}
				System.out.println();
			}
		}
		
		if (prevTHE != null) {
			//search for have
			have = getByHave(prevTHE.getEndNode(), name);
			if (have != null) return have;
		}

		return null;
	}
	
	private Relationship getByHave(Node context, final String name) {
		for (Relationship tdR : td_eval.traverse(context).relationships()) {
			
			System.out.println("GET check = "+tdR);
			
			if (name.equals(name(tdR)))
				return tdR;
		}
		
		return null;
	}
	
	public Relationship getByTraversal(final Relationship start_op, final PropertyContainer context, final String name) {
		
		TraversalDescription td;
		
		if (context instanceof Relationship) {
			td = Traversal.description().depthFirst().
			uniqueness(Uniqueness.RELATIONSHIP_PATH).
			evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
				@Override
				public Evaluation evaluate(Path path) {
					if (path.length() > 0) {
						Relationship r = path.lastRelationship(); 
						if (r.getStartNode().equals(path.endNode())) {
							if (r.equals(context)) {
								return INCLUDE_AND_PRUNE;
							} 
							return EXCLUDE_AND_CONTINUE;	
						} 
						return EXCLUDE_AND_PRUNE;
					}
					return EXCLUDE_AND_CONTINUE;
				}
			});
		} else {
			td = Traversal.description().depthFirst().
			uniqueness(Uniqueness.RELATIONSHIP_PATH).
			evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
				@Override
				public Evaluation evaluate(Path path) {
					if (path.length() > 0) {
						Relationship r = path.lastRelationship(); 
						if (r.getStartNode().equals(path.endNode())) {
							if (r.getEndNode().equals(context)) {
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
		
		System.out.println("context = "+context+" start_op = "+start_op);
		
		Node node = Utils.getByREF(start_op.getEndNode());
		for (Path path : td.traverse(node)) {
			System.out.println("path = "+path);
		}
		
		int deep = Integer.MAX_VALUE;
		Relationship result = null;

		Relationship thisResult = null;
		int thisDeep = 0;
		
		for (Path path : td.traverse(node)) {
			
			boolean foundIS = false;
			thisDeep = 0;
			for (Relationship r : path.relationships()) {
				if (thisDeep > 0) {
					thisDeep++;
					continue;
				}
				String type = r.getType().name();
				
				if (type.equals(IS._.relationshipType().name()) && name.equals(IS._.name(r))) {
					foundIS = true;
					
				} else if (type.equals(HAVE._.relationshipType().name()) && (name.equals(HAVE._.name(r)) || foundIS)) {
					//ignore empty have 
					if (!Utils.haveContext(r.getEndNode()))
						break;
					
					thisResult = r;
					thisDeep++;
				
				} else if (type.equals(IC._.relationshipType().name()) && (name.equals(IC._.name(r)) || foundIS)) {
					if (foundIS) {
						//store
						final Node sNode;
						if (context instanceof Relationship) {
							sNode = ((Relationship)context).getEndNode();
						} else {
							sNode = (Node)context;
						}
						final Node eNode = r.getEndNode();

						thisResult = AnimoGraph.execute(new GraphOperation<Relationship>() {
							@Override
							public Relationship execute() {
								Relationship res = sNode.createRelationshipTo(eNode, HAVE._.relationshipType());
								//RID.set(res, r.getId());
								return res;
							}
						});
						thisDeep++;
					}
				}
			}
			
			if (thisDeep < deep)
				result = thisResult;
		}
		
		return result;
	}
}
