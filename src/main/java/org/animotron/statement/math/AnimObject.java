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
package org.animotron.statement.math;

import java.io.IOException;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import javolution.util.FastList;

import org.animotron.expression.JExpression;
import org.animotron.graph.index.Order;
import org.animotron.io.Pipe;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.THE;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.index.IndexHits;

import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class AnimObject implements Relationship {
	
	protected static AnimObject ONE = new AnimObject(new JExpression(value(Long.valueOf(1))));
	protected static AnimObject MINUS_ONE = new AnimObject(new JExpression(value(Long.valueOf(-1))));
	
	PFlow pf = null;
	Relationship r;
	List<Relationship> elements = null;

	public AnimObject(Relationship r) {
		this.r = r;
	}

	public AnimObject(List<Relationship> elements) {
		this.elements = elements;
		
		//TODO: this.r = r;
	}

	public Relationship getRelationship() {
		return r;
	}
	
	private boolean check(Relationship r) {
    	if (r.isType(VALUE._)) {
    		elements.add(r);
    		return true;
    		
    	} else if (r.isType(AN._)) {
    		if (!Utils.haveContext(r.getEndNode())) {
        		elements.add( r.getEndNode().getSingleRelationship(REF._, Direction.OUTGOING) );
        		return true;
    		} else {
                try {
                	Relationship ref = r.getEndNode().getSingleRelationship(REF._, Direction.OUTGOING);
    				Object obj = THE._.reference(ref.getEndNode());
    				if (obj != null && obj instanceof String ) {
    					Statement s = Statements.name((String) obj);
    					if (s instanceof MathOperator) {
							elements.add(new AnimObject(r));
							return true;
    					}
    				} else {
    					//XXX: log
    					System.out.println("WARNING: REF but no name");
    				}
                } catch (Throwable t){}							
    		}
			throw new RuntimeException("Unsupported operator "+r);
    	}
    	return false; 
	}

	protected List<Relationship> getElements() throws IOException {
		if (elements == null) {
			elements = new FastList<Relationship>();

            IndexHits<Relationship> hits = Order._.context(r.getEndNode());
            try {
                for (Relationship r : hits) {
                	if (!check(r)) {
                		if (pf == null)
                			throw new RuntimeException("Unsupported operator "+r);
                		
                		Pipe pipe = Utils.getTheRelationships(pf, pf.getVector().question(r));
	                	QCAVector v;
	                	while ((v = pipe.take()) != null) {
	                		if (!check(v.getClosest()))
	                			throw new RuntimeException("Unsupported operator "+r);
	                	}
                	}
                }
            } finally {
            	hits.close();
            }
			
		}
		if (elements.size() == 1 && elements.get(0) instanceof AnimObject)
			elements = ((AnimObject)elements.get(0)).getElements();
			
		return elements;
	}

	public AnimObject sum(AnimObject b) throws IOException {
		System.out.println("+");
		
		List<Relationship> As = getElements();
		List<Relationship> Bs = b.getElements();
		
		System.out.println("As = ");
		System.out.println(Arrays.toString(As.toArray()));
		System.out.println("Bs = ");
		System.out.println(Arrays.toString(Bs.toArray()));
		
		if (As.size() == Bs.size()) {
			List<Relationship> eq = new FastList<Relationship>();
			
			Iterator<Relationship> it = As.iterator();
			while (it.hasNext()) {
				Relationship r = it.next();
				if (Bs.contains(r)) {
					eq.add(r);
					
					it.remove();
					Bs.remove( Bs.indexOf(r) );
				}
			}
			
			if (As.size() == 1 && As.size() == Bs.size()) {
				eq.add(sum(As.get(0), Bs.get(0)));
			}

			System.out.println(Arrays.toString(eq.toArray()));
			
			return new AnimObject(eq);
		}
		return null; //new AnimObject(this, b);
	}
	
	private Relationship sum(Relationship a, Relationship b) {
		if (a.isType(VALUE._) && b.isType(VALUE._)) {
			Number Na = VALUE.number(VALUE._.reference(a));
			Number Nb = VALUE.number(VALUE._.reference(b));
			
			Number result;
			if (Na instanceof Long && Nb instanceof Long) {
				result = Na.longValue() + Nb.longValue();
			} else {
				result = Na.doubleValue() + Nb.doubleValue();
			}
			
			return new JExpression(value(result));
		}
		throw new RuntimeException("not supported relations "+a+" & "+b);
	}

	public AnimObject sub(AnimObject b) {
		// TODO Auto-generated method stub
		return null;
//        if (a instanceof Long && b instanceof Long) {
//            return a.longValue() - b.longValue();
//        } else {
//            return a.doubleValue() - b.doubleValue();
//        }
	}

	public AnimObject mul(AnimObject b) {
		return null;
//        if (a instanceof Long && b instanceof Long) {
//            return a.longValue() * b.longValue();
//        } else {
//            return a.doubleValue() * b.doubleValue();
//        }
	}

	public AnimObject div(AnimObject b) {
		return null;
	}

	@Override
	public GraphDatabaseService getGraphDatabase() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean hasProperty(String key) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Object getProperty(String key) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Object getProperty(String key, Object defaultValue) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setProperty(String key, Object value) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Object removeProperty(String key) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Iterable<String> getPropertyKeys() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Iterable<Object> getPropertyValues() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public long getId() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void delete() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Node getStartNode() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Node getEndNode() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Node getOtherNode(Node node) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Node[] getNodes() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public RelationshipType getType() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isType(RelationshipType type) {
		// TODO Auto-generated method stub
		return false;
	}
}