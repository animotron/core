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
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.Reference;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.util.Set;

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
        return question;
    }

    private OnQuestion question = new OnQuestion() {
        @Override
        public void onMessage(final PFlow pf) {
        	final Relationship op = pf.getOP();
			//System.out.println("ALL **************************");
            
            for (QCAVector theVector : Utils.getByREF(pf, op)) {
            	
            	Relationship the = theVector.getAnswer();

    			//check, maybe, result was already calculated
    			if (!Utils.results(the, pf)) {
    				
    				Node node = the.getEndNode();

	            	Set<Node>[] lists = getUSEs(pf, node);
					Set<Node> uses = lists[1];
					Set<Node> directed = lists[2];
					
					System.out.println(uses);
					
					boolean underUSE = false;
					if (directed != null && directed.size() == 1) { 
						underUSE = true;
						node = directed.iterator().next();
					}
		
					if (underUSE && filtering(pf, the, uses))
		            	try {
		            		pf.sendAnswer( getThe(node) );
		            	} catch (Exception e) {}
					
			        for (Path path : td_IS_leaf.traverse(node)) {
			        	
			        	System.out.println(path);

			        	Relationship r = path.lastRelationship();
			        	if (!Utils.haveContext(r.getEndNode())) {
			        		
			        		//XXX: need better check, it can be reference from other then AN
			        		if (r.getStartNode().hasRelationship(Direction.INCOMING, REF._))
			        			continue;

		        			try {
				        		Relationship res = getThe(r.getStartNode());
			        			if (filtering(pf, res, uses)) {
				        			try {
				        				pf.sendAnswer( res );
				        			} catch (Exception e) {}
			        			}
		        			} catch (Exception e) {
							}
			        	} else {
			    			IndexHits<Relationship> hits = Order.queryDown(r.getEndNode());
			    			try {
			    				boolean first = true;
			    				for (Relationship res : hits) {
			    					if (first) {
			    						first = false;
			    						continue;
			    					}
			    					
			    					if (res.isType(REF._)) continue;
			    					
			    					if (res.isType(AN._) || res.isType(VALUE._)) {
			    						if (filtering(pf, res, uses)) {
					        				pf.sendAnswer( res );
			    						}
			    					}
			    				}
			    			} finally {
			    				hits.close();
			    			}
			        	}
			        }
    			}
            }
            pf.done();
        }
    };
}