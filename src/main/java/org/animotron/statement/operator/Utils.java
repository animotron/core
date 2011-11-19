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
package org.animotron.statement.operator;

import java.io.IOException;
import java.util.List;

import javolution.util.FastList;

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.index.Order;
import org.animotron.graph.index.Result;
import org.animotron.io.PipedInput;
import org.animotron.io.PipedOutput;
import org.animotron.manipulator.ACQVector;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.Suffix;
import org.animotron.statement.relation.IS;
import org.jetlang.channels.Subscribable;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;

import static org.animotron.Properties.CID;
import static org.animotron.Properties.RID;
import static org.animotron.graph.AnimoGraph.getDb;
import static org.animotron.graph.RelationshipTypes.REF;
import static org.animotron.graph.RelationshipTypes.RESULT;
import static org.neo4j.graphdb.Direction.INCOMING;
import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Utils {

	public static TraversalDescription td_RESULT = 
		Traversal.description().
			breadthFirst().
			relationships(RESULT, OUTGOING );
			//.evaluator(Evaluators.excludeStartPosition());

	public static TraversalDescription td_eval_IS = 
			Traversal.description().
				breadthFirst().
				relationships(IS._, OUTGOING);
				//relationships(IC._.relationshipType(), OUTGOING);
		
	public static TraversalDescription upIS = 
			Traversal.description().
				breadthFirst().
				relationships(IS._, INCOMING);
				//relationships(IC._.relationshipType(), OUTGOING);


//	public static PipedInput<ACQVector> getREFs(PFlow pf, final Node node) {
//		PipedOutput<ACQVector> out = new PipedOutput<ACQVector>(); 
//		PipedInput<ACQVector> in = out.getInputStream();
//
//		//System.out.println(node);
//		try {
//			try {
//				Relationship theNode = THE.__((String)THE._.reference(node));
//
//				if (theNode != null) {
//					List<Relationship> res = new FastList<Relationship>();
//					res.add(theNode);
//					
//					return res;
//				}
//			} catch (Exception e) {
//			}
//
//			Relationship first = null;
//			List<Relationship> list = new FastList<Relationship>();
//			IndexHits<Relationship> hits = Order.queryDown(node);
//			try {
//				for (Relationship res : hits) {
//					if (first == null) first = res;
//					
//					if (res.isType(org.animotron.statement.operator.REF._))
//						list = getTheRelationships(pf, res, list);
//				}
//
//				if (first != null && list.isEmpty())
//					getTheRelationships(pf, first, list);
//				
//			} finally {
//				hits.close();
//			}
//			return list;
//
//		} catch (Exception e) {
//			pf.sendException(e);
//		}
//		return null;
//	}

	public static List<Relationship> getSuffixes(final Node node) {
		List<Relationship> list = null;
		IndexHits<Relationship> hits = Order.queryDown(node);
		try {
			for (Relationship res : hits) {
				Statement s = Statements.relationshipType(res);
				if (s instanceof Suffix) {
					if (list == null) list = new FastList<Relationship>();
					list.add(res);
				}
			}
		} finally {
			hits.close();
		}
		return list;
	}
	
	public static PipedInput<ACQVector> getByREF(PFlow pf) {
		return getByREF(pf, pf.getOP());
	}

	public static PipedInput<ACQVector> getByREF(PFlow pf, final Relationship op) {
		PipedOutput<ACQVector> out = new PipedOutput<ACQVector>(); 
		PipedInput<ACQVector> in = out.getInputStream();
		
		Node node = op.getEndNode();
		
		//System.out.println(node);
		try {
			try {
				Relationship theNode = THE.__((String)THE._.reference(node));

				if (theNode != null) {
					out.write(new ACQVector(null, theNode));
					out.close();
					return in;
				}
			} catch (Exception e) {
			}

			Relationship first = null;
			IndexHits<Relationship> hits = Order.queryDown(node);
			try {
				for (Relationship res : hits) {
					if (first == null) first = res;
					
					if (res.isType(org.animotron.statement.operator.REF._))
						evaluable(pf, res, out);
				}
				
				if (first != null && out.isEmpty())
					evaluable(pf, first, out);
				
			} finally {
				hits.close();
			}

			out.close();
			return in;

		} catch (Exception e) {
			pf.sendException(e);
		}
		return null;
	}

	private static PipedOutput<ACQVector> evaluable(PFlow pf, Relationship r, PipedOutput<ACQVector> out) throws InterruptedException, IOException {
		
		Statement s = Statements.relationshipType(r);
		if (s instanceof Query || s instanceof Evaluable) {
			//System.out.println("+++++++++++++++++++++++++++++++++++++++++ get evaluable");
			PipedInput<ACQVector> in = Evaluator._.execute(new PFlow(pf), r);
			for (ACQVector e : in) {
				PFlow _pf_ = new PFlow(pf);
				_pf_.addContextPoint(e);
				
				Relationship result = e.getAnswer();
				
				if (result.isType(REF) 
						|| result.isType(org.animotron.statement.operator.REF._)
						|| result.isType(THE._)
					) {
					out.write(e);
				} else {
					for (ACQVector rr : eval(_pf_, result)) {
						out.write(rr);
					}
				}
			}
			//System.out.println("end++++++++++++++++++++++++++++++++++++++ get evaluable");
		} else {
			out.write(new ACQVector(null, r));
		}
		
		return out;
	}
	
	public static PipedInput<ACQVector> getTheRelationships(PFlow pf, Relationship r) throws IOException {
		PipedOutput<ACQVector> out = new PipedOutput<ACQVector>(); 
		PipedInput<ACQVector> in = out.getInputStream();

		getTheRelationships(pf, r, out);
		out.close();
		
		return in;
	}

	public static void getTheRelationships(PFlow pf, Relationship r, PipedOutput<ACQVector> out) {
		try {
		
			Statement s = Statements.relationshipType(r);
			if (s instanceof Query || s instanceof Evaluable) {
				//System.out.println("+++++++++++++++++++++++++++++++++++++++++ get evaluable");
				PipedInput<ACQVector> in = Evaluator._.execute(new PFlow(pf), r);
				for (ACQVector e : in) {
					PFlow _pf_ = new PFlow(pf);
					_pf_.addContextPoint(e);
					
					Relationship result = e.getAnswer();
					
					if (result.isType(REF) 
							|| result.isType(org.animotron.statement.operator.REF._)
							|| result.isType(THE._)
						) {
						out.write(e);
					} else {
						for (ACQVector rr : eval(_pf_, result)) {
							out.write(rr);
						}
					}
				}
				//System.out.println("end++++++++++++++++++++++++++++++++++++++ get evaluable");
			} else {
				out.write(new ACQVector(null, r));
			}
		} catch (Exception e) {
			pf.sendException(e);
		}
	}
	
	public static PipedInput<ACQVector> eval(PFlow pf, Relationship op) throws IOException {
        final PipedOutput<ACQVector> out = new PipedOutput<ACQVector>();
        PipedInput<ACQVector> in = out.getInputStream();

		IndexHits<Relationship> hits = Order.queryDown(op.getEndNode());
		for (Relationship rr : hits) {
			if (rr.isType(REF) || rr.isType(org.animotron.statement.operator.REF._)) continue;
			
			Statement _s = Statements.relationshipType(rr);
			if (_s instanceof Query || _s instanceof Evaluable) {
				PipedInput<ACQVector> _in = Evaluator._.execute(new PFlow(pf), rr);
				for (ACQVector ee : _in) {
					out.write(ee);
				}
				
			} else {
				out.write(new ACQVector(op, rr));
			}
		}
		out.close();
		
		return in;
	}

	public static Node getSingleREF(final Node node) {
		try {
			try {
				Node theNode = THE._((String)THE._.reference(node));

				if (theNode != null)
					return theNode;

			} catch (Exception e) {
			}

			final Relationship res = Order.first(1, node)[0];
			
			return res.getEndNode();

		} catch (IndexOutOfBoundsException e) {
		}
		return null;
	}

	@Deprecated //???
	public static Relationship getByREF(final Relationship r) {
		final Relationship res = Order.first(1, r.getEndNode())[0];
		
		return res;
	}

	public static boolean results(PFlow pf) {
		return results(pf.getOP(), pf, pf.getPathHash());
	}

	public static boolean results(PFlow pf, byte[] hash) {
		return results(pf.getOP(), pf, hash);
	}

	public static boolean results(Relationship op, PFlow pf) {
		return results(op, pf, pf.getPathHash());
	}

	public static boolean results(Relationship op, PFlow pf, byte[] hash) {
		boolean haveSome = false;

		//System.out.println("check index "+r+" "+pf.getPathHash()[0]+" "+pf.getPFlowPath());
		for (ACQVector v : Result.get(hash, op)) {
			pf.sendAnswer(v);
			
			haveSome = true;
		}
//		if (haveSome)
//			System.out.println("CACHED RESULT FOUND");
		
//		for (Relationship res : node.getRelationships(OUTGOING)) {
//			
//			if (res.getType().reference().startsWith("RESULT")) {
//				//System.out.println("GET result = "+res);
//				
//				pf.sendAnswer(res);
//				
//				haveSome = true;
//			}
//		}
		return haveSome;
	}
	
	public static boolean haveContext(Node node) {
		
		for (Relationship r : node.getRelationships(OUTGOING)) {
			
			if (r.isType(REF)) {
				//ignore REF
			} else {
				return true;
			}
		}
		
		return false;
	}

	public static boolean haveContext(PFlow pf) {
		
		IndexHits<Relationship> q = Order.queryDown(pf.getOPNode());
		try {
			while (q.hasNext()) {
				Relationship r = q.next();
				Statement s = Statements.relationshipType(r);
				if (r.isType(org.animotron.statement.operator.REF._)
					|| r.isType(REF)
					|| s instanceof Suffix) 
					
					continue;
				
				Subscribable<PFlow> onQuestion = pf.getManipulator().onQuestion(r);
				
				if (onQuestion != null) {
					return true;
					
				} else {
					return true;
				}
			}
			
			return false;
		} finally {
			q.close();
		}
	}

	public static Relationship relax(Relationship relation) {
		Relationship r = relation;
		while (true) {
			try {
	        	r = getDb().getRelationshipById(
	                (Long)r.getProperty(RID.name())
	            );
			} catch (Exception ex) {
				break;
			}
		}
		return r;
	}

	public static Relationship createResult(final PFlow pf, final Node node, final Relationship r, final RelationshipType rType) {
		return createResult(pf, null, node, r, rType, pf.getPathHash());
	}

	public static Relationship createResult(final PFlow pf, final ACQVector context, final Node node, final Relationship r, final RelationshipType rType) {
		return createResult(pf, context, node, r, rType, pf.getPathHash());
	}

	public static Relationship createResult(final PFlow pf, final Node node, final Relationship r, final RelationshipType rType, final byte[] hash) {
		return createResult(pf, null, node, r, rType, hash);
	}

	public static Relationship createResult(final PFlow pf, final ACQVector context, final Node node, final Relationship r, final RelationshipType rType, final byte[] hash) {
		return AnimoGraph.execute(new GraphOperation<Relationship>() {
			@Override
			public Relationship execute() {
				//check if it exist
				Relationship res = Result.getIfExist(node, r, rType);
				if (res != null) {
					Result.add(res, hash);
					
					//for debug
					//CID.set(res, context.getId());
					
					return res;
				}
				
				//adding if not
				res = node.createRelationshipTo(r.getEndNode(), rType);
				//store to relationship arrow
				RID.set(res, r.getId());
				//for debug
				if (context != null)
					CID.set(res, context.mashup());
				Result.add(res, hash);
				//System.out.println("add to index "+r+" "+pf.getPathHash()[0]+" "+pf.getPFlowPath());
				return res;
			}
		});
	}
}