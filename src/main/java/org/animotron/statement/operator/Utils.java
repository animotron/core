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

import javolution.util.FastList;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.index.Order;
import org.animotron.graph.index.Result;
import org.animotron.io.PipedInput;
import org.animotron.io.PipedOutput;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.Suffix;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;

import java.io.IOException;
import java.util.List;
import java.util.Set;

import static org.animotron.Properties.*;
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
				relationships(AN._, OUTGOING);
				//relationships(IC._.relationshipType(), OUTGOING);
		
	public static TraversalDescription upIS = 
			Traversal.description().
				breadthFirst().
				relationships(AN._, INCOMING);
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

	@Deprecated
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
	
	public static PipedInput<QCAVector> getByREF(PFlow pf) {
		return getByREF(pf, pf.getOP());
	}

	public static PipedInput<QCAVector> getByREF(PFlow pf, final Relationship op) {
		PipedOutput<QCAVector> out = new PipedOutput<QCAVector>(); 
		PipedInput<QCAVector> in = out.getInputStream();
		
		Node node = op.getEndNode();
		
		//System.out.println(node);
		try {
			try {
				Relationship theNode = THE.__((String)THE._.reference(node));

				if (theNode != null) {
					out.write(pf.getVector().answered(theNode));
					out.close();
					return in;
				}
			} catch (Exception e) {
			}

			Relationship first = null;
			IndexHits<Relationship> hits = Order.queryDown(node);
			try {
				for (Relationship res : hits) {
					if (res.isType(org.animotron.statement.operator.REF._) || first == null) {
						evaluable(pf, pf.getVector().question(res), out, op);
						if (first == null)
							first = res;
					} else
						break;
				}
				
//				if (first != null && out.isEmpty())
//					evaluable(pf, first, out, op);
				
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

	private static PipedOutput<QCAVector> evaluable(final PFlow pf, final QCAVector v, final PipedOutput<QCAVector> out, final Relationship op) throws InterruptedException, IOException {
		
		Relationship r = v.getClosest();
		Statement s = Statements.relationshipType(r);
		if (s instanceof Query || s instanceof Evaluable) {
			//System.out.println("+++++++++++++++++++++++++++++++++++++++++ get evaluable");
			PipedInput<QCAVector> in = Evaluator._.execute(new PFlow(pf), v);
			for (QCAVector e : in) {
				PFlow _pf_ = new PFlow(pf);
				//_pf_.addContextPoint(e);
				
                Statement aS = Statements.relationshipType(e.getAnswer());
				if (!(aS instanceof Evaluable && !(s instanceof Shift))) {
//				if (result.isType(REF) 
//						|| result.isType(org.animotron.statement.operator.REF._)
//						|| result.isType(THE._)
//					) {
					out.write(e);
				} else {
					for (QCAVector rr : eval(_pf_, e)) {
						out.write(rr);
					}
				}
			}
			//System.out.println("end++++++++++++++++++++++++++++++++++++++ get evaluable");
		} else {
			out.write(v.getContext().get(0).answered(r));
		}
		
		return out;
	}
	
	public static PipedInput<QCAVector> getTheRelationships(PFlow pf, Relationship r) throws IOException {
		PipedOutput<QCAVector> out = new PipedOutput<QCAVector>(); 
		PipedInput<QCAVector> in = out.getInputStream();

		getTheRelationships(pf, r, out);
		out.close();
		
		return in;
	}

	public static void getTheRelationships(PFlow pf, Relationship r, PipedOutput<QCAVector> out) {
		try {
		
			Statement s = Statements.relationshipType(r);
			if (s instanceof AN) {
                try {
                    s = Statements.name((String) THE._.reference(r));
                    
                } catch (Exception e) {
    				out.write(new QCAVector(null, r));
    				return;
                }
			}

			if (s instanceof Query || s instanceof Evaluable) {
				//System.out.println("+++++++++++++++++++++++++++++++++++++++++ get evaluable");
				PipedInput<QCAVector> in = Evaluator._.execute(new PFlow(pf), r);
				for (QCAVector e : in) {
					PFlow _pf_ = new PFlow(pf);
					_pf_.addContextPoint(e);
					
					Relationship result = e.getUnrelaxedAnswer();
					
					if (result.isType(REF) 
							|| result.isType(org.animotron.statement.operator.REF._)
							|| result.isType(THE._)
						) {
						out.write(e);
					} else {
						for (QCAVector rr : eval(_pf_, e)) {
							out.write(rr);
						}
					}
				}
				//System.out.println("end++++++++++++++++++++++++++++++++++++++ get evaluable");
			} else {
				out.write(new QCAVector(null, r));
			}
		} catch (Exception e) {
			pf.sendException(e);
		}
	}
	
	public static PipedInput<QCAVector> eval(PFlow pf, QCAVector vector) throws IOException {
        final PipedOutput<QCAVector> out = new PipedOutput<QCAVector>();
        PipedInput<QCAVector> in = out.getInputStream();
        
        Relationship op = vector.getClosest();
        
        QCAVector prev = null;

		IndexHits<Relationship> hits = Order.queryDown(op.getEndNode());
		for (Relationship rr : hits) {
			if (rr.isType(REF) || rr.isType(org.animotron.statement.operator.REF._)) continue;
			
			Statement _s = Statements.relationshipType(rr);
			if (_s instanceof Query || _s instanceof Evaluable) {
				prev = vector.question(rr, prev);
				PipedInput<QCAVector> _in = Evaluator._.execute(new PFlow(pf), prev);
				for (QCAVector ee : _in) {
					//XXX: ee should be context too???
					out.write(new QCAVector(op, vector, ee.getUnrelaxedAnswer()));
				}
				
			} else {
				out.write(new QCAVector(op, vector, rr));
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
		for (QCAVector v : Result.get(hash, op)) {
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
	
	public static boolean haveContext(PFlow pf) {
		return haveContext(pf.getOPNode());
	}
	
	public static boolean haveContext(Node node) {
		return CONTEXT.has(node);
		
//		IndexHits<Relationship> q = Order.queryDown(node);
//		try {
//			while (q.hasNext()) {
//				Relationship r = q.next();
//				
//				Statement s = Statements.relationshipType(r);
//				if (r.isType(org.animotron.statement.operator.REF._)
//					|| r.isType(REF)
//					|| s instanceof Suffix) 
//					
//					continue;
//				
//				Subscribable<PFlow> onQuestion = Evaluator._.onQuestion(r);
//				
//				if (onQuestion != null) {
//					return true;
//					
//				} else {
//					return true;
//				}
//			}
//			
//			return false;
//		} finally {
//			q.close();
//		}
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

	public static long relaxedId(Relationship relation) {
		try {
            return (Long)relation.getProperty(RID.name());
		} catch (Exception ex) {
		}
		return -1;
	}

	public static Relationship createResult(final PFlow pf, final Node node, final Relationship r, final RelationshipType rType) {
		return createResult(pf, null, node, r, rType, pf.getPathHash());
	}

	public static Relationship createResult(final PFlow pf, final List<QCAVector> context, final Node node, final Relationship r, final RelationshipType rType) {
		return createResult(pf, context, node, r, rType, pf.getPathHash());
	}

	public static Relationship createResult(final PFlow pf, final Node node, final Relationship r, final RelationshipType rType, final byte[] hash) {
		return createResult(pf, null, node, r, rType, hash);
	}

	public static Relationship createResult(final PFlow pf, final List<QCAVector> context, final Node node, final Relationship r, final RelationshipType rType, final byte[] hash) {
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
				if (context != null) {
					if (context.size() > 1) System.out.println("WARNING ... more then one context for CID");
					//XXX: rewrite!
					for (QCAVector c : context) {
						try {
							CID.set(res, c.mashup());
						} catch (Exception e) {
						}
					}
				}
				Result.add(res, hash);
				//System.out.println("add to index "+r+" "+pf.getPathHash()[0]+" "+pf.getPFlowPath());
				return res;
			}
		});
	}

	public static void debug(Statement st, Relationship op, Set<Node> thes) {
		System.out.print(st.name()+" "+op.getId()+" ");
		for (Node theNode : thes) {
			try {
				System.out.print("'"+name(theNode)+"'");
			} catch (Exception e) {
				System.out.print("???");
			}
			System.out.print(" ["+theNode+"], ");
		}
		System.out.println();
	}
	
    public static String name(Node theNode) {
    	return (String)NAME.get(theNode);
    }
}