/*
 *  Copyright (C) 2011-2012 The Animo Project
 *  http://animotron.org
 *  	
 *  This file is part of Animotron.
 *  
 *  Animotron is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as 
 *  published by the Free Software Foundation, either version 3 of 
 *  the License, or (at your option) any later version.
 *  
 *  Animotron is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *  
 *  You should have received a copy of 
 *  the GNU Affero General Public License along with Animotron.  
 *  If not, see <http://www.gnu.org/licenses/>.
 */
package org.animotron.statement.query;

import org.animotron.graph.index.Order;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Reference;
import org.animotron.statement.operator.THE;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import javolution.util.FastSet;

/**
 * Query operator 'PREFER'.
 * 
 * Return nothing (if no USE) or 'all' USE. 
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class PREFER extends AbstractQuery implements Reference {

	public static final PREFER _ = new PREFER();

	private static boolean debug = false;

	private PREFER() { super("prefer", "~~~"); }

    public OnQuestion onCalcQuestion() {
        return question;
    }

    private OnQuestion question = new OnQuestion() {
        @Override
        public void onMessage(final PFlow pf) {

        	//if (debug) { 
				System.out.println("PREFER "+pf.getOP()+" "+pf.getVector());
				Utils.debug(PREFER._, pf);
			//}
        	
			if (!Utils.results(pf)) {
	            for (QCAVector v : AN.getREFs(pf, pf.getVector())) {
	            	
	            	Relationship ref = v.getAnswer(); 
	            	Node node = ref.getEndNode();
	
    				FastSet<Node> uses = FastSet.newInstance();
    				FastSet<Node> weaks = FastSet.newInstance();
    				FastSet<Path> directed = FastSet.newInstance();

    				try {
		            	getUSEs(pf, node, uses, weaks, directed);
		            	
		            	if (debug) {
							System.out.println(uses);
			            	for (Node n : uses) {
			            		System.out.println(THE._.reference(n));
			            	}
							System.out.println(weaks);
							System.out.println(directed);
			            	for (Path n : directed) {
			            		System.out.println(THE._.reference(n.endNode()));
			            	}
		            	}
						
						if (!uses.isEmpty() || !weaks.isEmpty()) {
				
							boolean underUSE = false;
							Node n = getClosestIntersection(directed);
		    				if (n != null) {
		    					node = n;
		    					underUSE = true;
		    				}

							Relationship res;
							if (underUSE 
									&& isLeaf(node) 
									&& (res = getThe(node)) != null  
									&& filtering(pf, res, uses, weaks))
								pf.sendAnswer( res );
				
					        for (Path path : td_IS_leaf.traverse(node)) {
					        	
					        	//System.out.println(path);
	
					        	Relationship r = path.lastRelationship();
					        	if (!Utils.haveContext(r.getEndNode())) {
					        		
					        		//XXX: need better check, it can be reference from other then AN
					        		if (!isLeaf(r.getStartNode()))
					        			continue;
	
					        		try {
					        			res = getThe(r.getStartNode());
					        			if (filtering(pf, res, uses, weaks)) {
					        				pf.sendAnswer( res );
					        			}
					        		} catch (Exception e) {
				        				for (Path p : Utils.td_THE.traverse(r.getStartNode())) {
				        					res = p.lastRelationship();
						        			if (filtering(pf, res, uses, weaks)) {
						        				pf.sendAnswer( res );
						        			}
				        				}
									}
					        	} else {
					    			IndexHits<Relationship> hits = Order.context(r.getEndNode());
					    			try {
					    				for (Relationship rr : hits) {
					    					
					    					if (rr.isType(AN._) || rr.isType(VALUE._)) {
					    						if (filtering(pf, rr, r.getEndNode(), uses, weaks)) {
							        				pf.sendAnswer( rr );
					    						}
					    					}
					    				}
					    			} finally {
					    				hits.close();
					    			}
					        	}
					        }
						}
    				} finally {
    					FastSet.recycle(uses);
    					FastSet.recycle(weaks);
    					FastSet.recycle(directed);
    				}
	            }
			}
            pf.done();
        }
    };
}