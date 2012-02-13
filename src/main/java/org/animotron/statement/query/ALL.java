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

import java.util.Arrays;
import java.util.Set;

import org.animotron.graph.index.Order;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Reference;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import javolution.util.FastSet;

/**
 * Query operator 'ALL'.
 * 
 * Return perfect 'all' USE.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class ALL extends AbstractQuery implements Reference {
	
	public static final ALL _ = new ALL();
	
	private ALL() { super("all", "~~"); }

    public OnQuestion onCalcQuestion() {
        return new Calc();
    }
    
    class Calc extends OnQuestion {
        @Override
        public void act(final PFlow pf) {
			//System.out.println("ALL **************************");
            
        	Relationship the = null;
        	FastSet<Relationship> thes = FastSet.newInstance();
			FastSet<Relationship> others = FastSet.newInstance();
			try {
				Utils.getTHELikeBag(pf, pf.getVector(), thes);

				if (thes.isEmpty()) return;
				
				FastSet.Record rec = thes.head();
				rec = rec.getNext();
				the = thes.valueOf(rec);
				
				Node node = the.getEndNode();

				FastSet<Node> uses = FastSet.newInstance();
				FastSet<Node> weaks = FastSet.newInstance();
				FastSet<Path> directed = FastSet.newInstance();

				try {
    				getUSEs(pf, node, uses, weaks, directed);
    				
    				for (FastSet.Record end = thes.tail(); (rec = rec.getNext()) != end;) {
    					uses.add(thes.valueOf(rec).getEndNode());
    				}
					
					//System.out.println(uses);
					
					Set<Relationship> list = getExpected(pf);
					if (list !=null && !list.isEmpty()) {
						System.out.println("after predicate "+Arrays.toString(list.toArray()));
						for (Relationship r : list) {
							if (setFiltering(r.getEndNode(), uses, weaks))
								if (isLeaf(r.getEndNode())) {
									//System.out.print("answered ");
									//Utils.debug(r);
			        				pf.sendAnswer( r );
								}
						}
						return;
					}

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
						
		            	try {
		            		pf.sendAnswer( res );
		            	} catch (Exception e) {}
					
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
				} finally {
					FastSet.recycle(uses);
					FastSet.recycle(weaks);
					FastSet.recycle(directed);
				}
			} finally {
				FastSet.recycle(thes);
				FastSet.recycle(others);
			}
        }
    }
}