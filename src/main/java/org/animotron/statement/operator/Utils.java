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

import org.animotron.graph.index.Order;
import org.animotron.graph.index.Result;
import org.animotron.io.PipedInput;
import org.animotron.io.PipedOutput;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.jetlang.channels.Subscribable;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;

import static org.animotron.Properties.RID;
import static org.animotron.graph.AnimoGraph.getDb;
import static org.animotron.graph.RelationshipTypes.REF;
import static org.animotron.graph.RelationshipTypes.RESULT;
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


	public static List<Relationship> getREFs(PFlow pf, final Node node) {
		//System.out.println(node);
		try {
			try {
				Relationship theNode = THE.__((String)THE._.reference(node));

				if (theNode != null) {
					List<Relationship> res = new FastList<Relationship>();
					res.add(theNode);
					
					return res;
				}
			} catch (Exception e) {
			}

			List<Relationship> list = new FastList<Relationship>();
			IndexHits<Relationship> hits = Order.queryDown(node);
			try {
				for (Relationship res : hits) {
					if (res.isType(org.animotron.statement.operator.REF._))
						list = getTheRelationships(pf, res, list);
				}
				
			} finally {
				hits.close();
			}
			return list;

		} catch (Exception e) {
			pf.sendException(e);
		}
		return null;
	}

	public static List<Node> getByREF(PFlow pf, final Node node) {
		//System.out.println(node);
		try {
			try {
				Node theNode = THE._((String)THE._.reference(node));

				if (theNode != null) {
					List<Node> res = new FastList<Node>();
					res.add(theNode);
					
					return res;
				}
			} catch (Exception e) {
			}

			List<Node> list = new FastList<Node>();
			IndexHits<Relationship> hits = Order.queryDown(node);
			try {
				for (Relationship res : hits) {
					if (res.isType(org.animotron.statement.operator.REF._))
						evaluable(pf, res, list);
				}
				
			} finally {
				hits.close();
			}
			return list;

		} catch (Exception e) {
			pf.sendException(e);
		}
		return null;
	}

	private static List<Node> evaluable(PFlow pf, Relationship r, List<Node> list) throws InterruptedException, IOException {
		
		Statement s = Statements.relationshipType(r);
		if (s instanceof Query || s instanceof Evaluable) {
			//System.out.println("+++++++++++++++++++++++++++++++++++++++++ get evaluable");
			PipedInput<Relationship[]> in = Evaluator._.execute(new PFlow(pf), r);
			for (Relationship[] e : in) {
				PFlow _pf_ = new PFlow(pf);
				for (int i = 1; i < e.length; i++)
					if (e[i] != null) _pf_.addContextPoint(e[i]);
				
				Relationship result = Utils.relax(e[0]);
				
				if (result.isType(REF) 
						|| result.isType(org.animotron.statement.operator.REF._)
						|| result.isType(THE._)
					) {
					list.add(result.getEndNode());
				} else {
					for (Relationship[] rr : eval(_pf_, result)) {
						list.add(rr[0].getEndNode());
					}
				}
			}
			//System.out.println("end++++++++++++++++++++++++++++++++++++++ get evaluable");
		} else {
			list.add(r.getEndNode());
		}
		
		return list;
	}
	
	public static List<Relationship> getTheRelationships(PFlow pf, Relationship r) {
		return getTheRelationships(pf, r, new FastList<Relationship>());
	}

	public static List<Relationship> getTheRelationships(PFlow pf, Relationship r, List<Relationship> list) {
		try {
		
			Statement s = Statements.relationshipType(r);
			if (s instanceof Query || s instanceof Evaluable) {
				//System.out.println("+++++++++++++++++++++++++++++++++++++++++ get evaluable");
				PipedInput<Relationship[]> in = Evaluator._.execute(new PFlow(pf), r);
				for (Relationship[] e : in) {
					PFlow _pf_ = new PFlow(pf);
					for (int i = 1; i < e.length; i++)
						if (e[i] != null) _pf_.addContextPoint(e[i]);
					
					Relationship result = Utils.relax(e[0]);
					
					if (result.isType(REF) 
							|| result.isType(org.animotron.statement.operator.REF._)
							|| result.isType(THE._)
						) {
						list.add(result);
					} else {
						for (Relationship[] rr : eval(_pf_, result)) {
							list.add(rr[0]);
						}
					}
				}
				//System.out.println("end++++++++++++++++++++++++++++++++++++++ get evaluable");
			} else {
				list.add(r);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return list;
	}
	
	public static PipedInput<Relationship[]> eval(PFlow pf, Relationship op) throws IOException {
        final PipedOutput<Relationship[]> out = new PipedOutput<Relationship[]>();
        PipedInput<Relationship[]> in = out.getInputStream();

		IndexHits<Relationship> hits = Order.queryDown(op.getEndNode());
		for (Relationship rr : hits) {
			if (rr.isType(REF) || rr.isType(org.animotron.statement.operator.REF._)) continue;
			
			Statement _s = Statements.relationshipType(rr);
			if (_s instanceof Query || _s instanceof Evaluable) {
				PipedInput<Relationship[]> _in = Evaluator._.execute(new PFlow(pf), rr);
				for (Relationship[] ee : _in) {
					ee[0] = Utils.relax(ee[0]);
					out.write(ee);
				}
				
			} else {
				out.write(new Relationship[] {rr});
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
		return results(pf.getOPNode(), pf, true);
	}

	public static boolean results(Node node, PFlow pf) {
		return results(node, pf, true);
	}

	public static boolean results(Node node, PFlow pf, boolean fullPath) {
		boolean haveSome = false;

		byte[] hash;
		if (fullPath)
			hash = pf.getPathHash();
		else
			hash = pf.getOpHash();
		
		//System.out.println("check index "+r+" "+pf.getPathHash()[0]+" "+pf.getPFlowPath());
		for (Relationship[] r : Result.get(hash, node)) {
			if (r.length == 1)
				pf.sendAnswer(r[0], pf.getOP());
			else
				pf.sendAnswer(r);
			
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
				if (r.isType(org.animotron.statement.operator.REF._)) continue;
				
				Subscribable<PFlow> onQuestion = pf.getManipulator().onQuestion(r);
				
				if (onQuestion != null) {
					return true;
					
				} else if (r.isType(REF)) {
					//ignore REF
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
}