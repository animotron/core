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

import javolution.util.FastSet;

import org.animotron.manipulator.PFlow;
import org.animotron.statement.operator.*;
import org.animotron.statement.operator.DEF;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.TraversalDescription;

import java.util.Arrays;
import java.util.Set;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractReference extends AbstractQuery implements Reference {

	private static boolean optimizer = false;
	
    public AbstractReference(String... name) {
        super(name);
    }

	public void process(final PFlow pf, boolean returnFirstOnly, boolean forceUSE) {
    	Relationship the = null;
    	FastSet<Relationship> thes = FastSet.newInstance();
    	final FastSet<Node> theNodes = FastSet.newInstance();
		FastSet<Relationship> others = FastSet.newInstance();
		try {
			Utils.getTHELikeBag(pf, pf.getVector(), thes);

			if (thes.isEmpty()) return;
			
			FastSet.Record rec = thes.head();
			rec = rec.getNext();
			the = thes.valueOf(rec);
			
			Node node = the.getEndNode();
			theNodes.add(node);

			FastSet<Node> uses = FastSet.newInstance();
			FastSet<Node> weaks = FastSet.newInstance();
			FastSet<Path> directed = FastSet.newInstance();

			try {
				getUSEs(pf, node, uses, weaks, directed);
				
				if (forceUSE && !(!uses.isEmpty() || !weaks.isEmpty())) {
					return;
				}

				for (FastSet.Record end = thes.tail(); (rec = rec.getNext()) != end;) {
					Node n = thes.valueOf(rec).getEndNode();
					
					theNodes.add(n);
					uses.add(n);
				}
				
//				System.out.println("uses");
//				System.out.println(Arrays.toString(uses.toArray()));
//				System.out.println("weaks");
//				System.out.println(Arrays.toString(weaks.toArray()));
				
        		Relationship res;

        		if (optimizer) {
	        		Set<Relationship> list = getExpected(pf);
					if (list !=null && !list.isEmpty()) {
						System.out.println("after predicate "+Arrays.toString(list.toArray()));
						for (Relationship r : list) {
							
							//System.out.println("*"+r);
							
					    	TraversalDescription trav = td.
									relationships(AN._, Direction.OUTGOING).
									relationships(REF._).
							evaluator(new ISearcher(){
								@Override
								public Evaluation evaluate(Path path) {
									return _evaluate_(path, theNodes);
								}
							});
							
							
							if (!trav.traverse(r.getEndNode()).iterator().hasNext()) {
								continue;
							}
							
							if (setFiltering(r.getEndNode(), uses, weaks)) {
								if (isLeaf(r.getEndNode())) {
									//System.out.print("answered ");
									//Utils.debug(r);
			        				pf.sendAnswer( r );
			        				
			        				if (returnFirstOnly) return;
								}
							}
					        for (Path path : td_IS_leaf.traverse(r.getEndNode())) {
					        	
					        	Node n = path.lastRelationship().getStartNode();
								if (setFiltering(n, uses, weaks)) {
									if (isLeaf(n) && (res = DEF._.get(n)) != null) {
										//System.out.print("answered ");
										//Utils.debug(r);
				        				pf.sendAnswer( res );
				        				
				        				if (returnFirstOnly) return;
									}
								}
					        }
	
							
						}
						return;
					}
				}
				//System.out.println("NOT OPTIMIZED");

				boolean underUSE = false;
//				Node n = getClosestIntersection(directed);
//				if (n != null) {
//					node = n;
//					underUSE = true;
//				}
				
				if (underUSE 
//						&& isLeaf(node) 
						&& (res = DEF._.get(node)) != null
						&& filtering(pf, res, uses, weaks))
					
	            	try {
	            		pf.sendAnswer( res );
        				if (returnFirstOnly) return;
	            	} catch (Throwable t) {}
				
		        for (Path path : td_IS_leaf.traverse(node)) {
		        	
//		        	System.out.println("-> "+path);

		        	Relationship r = path.lastRelationship();
		        	if (!Utils.haveContext(r.getEndNode())) {
		        		
//		        		//XXX: need better check, it can be reference from other then AN
//		        		if (!isLeaf(r.getStartNode()))
//		        			continue;

	        			try {
			        		res = DEF._.get(r.getStartNode());
			        		if (res != null) {
			        			if (filtering(pf, DEF._.actualRevision(res), uses, weaks)) {
			        				pf.sendAnswer( res );
			        				if (returnFirstOnly) return;
			        			}
			        		} else {
		        				for (Path p : Utils.td_THE.traverse(r.getStartNode())) {
		        					res = p.lastRelationship();
				        			if (filtering(pf, DEF._.actualRevision(res), uses, weaks)) {
				        				pf.sendAnswer( res );
				        				if (returnFirstOnly) return;
				        			}
		        				}
			        		}
	        			} catch (Throwable t) {
	        				pf.sendException(t);
	        				return;
						}
//		        	} else {
//		    			IndexHits<Relationship> hits = Order._.context(r.getEndNode());
//		    			try {
//		    				for (Relationship rr : hits) {
//		    					
//		    					if (rr.isType(AN._) || rr.isType(VALUE._)) {
//		    						if (filtering(pf, rr, r.getEndNode(), uses, weaks)) {
//				        				pf.sendAnswer( rr );
//				        				if (returnFirstOnly) return;
//		    						}
//		    					}
//		    				}
//		    			} finally {
//		    				hits.close();
//		    			}
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
