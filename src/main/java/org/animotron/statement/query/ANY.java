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
package org.animotron.statement.query;

import org.animotron.graph.index.Order;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Reference;
import org.animotron.statement.operator.Utils;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import javolution.util.FastSet;

/**
 * Query operator 'ANY'.
 * 
 * Return any first 'perfect' USE
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class ANY extends AbstractQuery implements Reference {
	
	public static final ANY _ = new ANY();
	
	private ANY() { super("any", "~"); }
	
	private static boolean debug = false;

	public OnQuestion onCalcQuestion() {
        return question;
    }

	private OnQuestion question = new OnQuestion() {
        @Override
        public void onMessage(final PFlow pf) {
            long ts = System.currentTimeMillis();

			if (debug) { 
				System.out.println("ANY "+pf.getOP()+" "+pf.getVector());
				Utils.debug(ANY._, pf);
			}
				
			for (QCAVector vector : Utils.getByREF(pf)) {
				Relationship the = vector.getClosest();
				
				if (!Utils.results(the, pf)) {
				
					Node node = the.getEndNode();
					
    				FastSet<Node> uses = FastSet.newInstance();
    				FastSet<Path> directed = FastSet.newInstance();

    				try {
						getUSEs(pf, node, uses, directed);
						
						if (debug) { 
							System.out.println(uses);
							System.out.println(directed);
    					}
						
						boolean underUSE = false;
						Node n = getClosestIntersection(directed);
	    				if (n != null) {
	    					node = n;
	    					underUSE = true;
	    				}

						if (debug) 
							System.out.println(" node = "+node);
						
						int i = 0;
			
		        		Relationship res;
						if (underUSE && ((res = getThe(node)) != null) && filtering(pf, res, uses))
			            	try {
			            		pf.sendAnswer( res );
			            		break;
			            	} catch (Exception e) {}
	
						for (Path path : td_IS_leaf.traverse(node)) {
			        		i++;
				        	Relationship r = path.lastRelationship();
	
				        	if (debug) System.out.println(path);
				        	if (debug) System.out.println("have context = "+Utils.haveContext(r.getEndNode()));
	
				        	if (!Utils.haveContext(r.getEndNode())) {
				        		
				        		//XXX: need better check, it can be reference from other then AN
				        		//if (r.getStartNode().hasRelationship(Direction.INCOMING, REF._))
				        		if (THE_by_REF.traverse(r.getStartNode()).iterator().hasNext()) { //XXX: sure that StartNode? 
						        	if (debug) System.out.println("continue");
				        			continue;
				        		}
	
				        		try {
			        				res = getThe(r.getStartNode());
				        			if (filtering(pf, res, uses)) {
				        				pf.sendAnswer( res );
				        				break;
				        			}
				        		} catch (Exception e) {
			        				for (Path p : Utils.td_THE.traverse(r.getStartNode())) {
			        					res = p.lastRelationship();
					        			if (filtering(pf, res, uses)) {
					        				pf.sendAnswer( res );
					        			}
			        				}
								}
				        	} else {
				    			IndexHits<Relationship> hits = Order.context(r.getEndNode());
				    			try {
				    				for (Relationship rr : hits) {
				    					
				    					if (rr.isType(AN._)) {
				    						if (filtering(pf, rr, r.getEndNode(), uses)) {
						        				pf.sendAnswer( rr );
						        				break;
				    						}
				    					}
				    				}
				    			} finally {
				    				hits.close();
				    			}
				        	}
				        }
						if (debug) System.out.println(i);
    				} finally {
    					FastSet.recycle(uses);
    					FastSet.recycle(directed);
    				}
		        }
			}
            pf.done();
            ts = System.currentTimeMillis() - ts;
            if (debug) System.out.println("ANY "+pf.getOP()+" "+ts);
        }
    };
}