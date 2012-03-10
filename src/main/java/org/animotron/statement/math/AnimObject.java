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

import javolution.util.FastList;
import org.animotron.expression.AbstractExpression;
import org.animotron.expression.JExpression;
import org.animotron.graph.builder.FastGraphBuilder;
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
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.List;

import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class AnimObject extends AbstractExpression {
	
	protected static AnimObject ONE = new AnimObject(new JExpression(value(Long.valueOf(1))));
	protected static AnimObject MINUS_ONE = new AnimObject(new JExpression(value(Long.valueOf(-1))));
	
	PFlow pf = null;
	Relationship r;
	List<Relationship> elements = null;
    Statement op = null;

	public AnimObject(Relationship r) {
        super(null);
		this.r = r;
	}

	public AnimObject(Statement op, List<Relationship> elements) {
        super(new FastGraphBuilder());
		this.elements = elements;
        this.op = op;
	}

	public AnimObject(Statement op, Relationship... elements) {
        super(new FastGraphBuilder());
        this.elements = new FastList();
        for (Relationship r : elements) {
		    this.elements.add(r);
        }
        this.op = op;
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

//	public static AnimObject sum(Relationship... elements) throws IOException {
//        return new AnimObject(SUM._, elements);
//		System.out.println("+");
//		
//		List<Relationship> As = getElements();
//		List<Relationship> Bs = b.getElements();
//		
//		System.out.println("As = ");
//		System.out.println(Arrays.toString(As.toArray()));
//		System.out.println("Bs = ");
//		System.out.println(Arrays.toString(Bs.toArray()));
//		
//		if (As.size() == Bs.size()) {
//			List<Relationship> eq = new FastList<Relationship>();
//			
//			Iterator<Relationship> it = As.iterator();
//			while (it.hasNext()) {
//				Relationship r = it.next();
//				if (Bs.contains(r)) {
//					eq.add(r);
//					
//					it.remove();
//					Bs.remove( Bs.indexOf(r) );
//				}
//			}
//			
//			if (As.size() == 1 && As.size() == Bs.size()) {
//				eq.add(sum(As.get(0), Bs.get(0)));
//			}
//
//			System.out.println(Arrays.toString(eq.toArray()));
//			
//			return new AnimObject(eq);
//		}
//		return null; //new AnimObject(this, b);
//	}
//
//	private Relationship sum(Relationship a, Relationship b) {
//		if (a.isType(VALUE._) && b.isType(VALUE._)) {
//			Number Na = VALUE.number(VALUE._.reference(a));
//			Number Nb = VALUE.number(VALUE._.reference(b));
//			
//			Number result;
//			if (Na instanceof Long && Nb instanceof Long) {
//				result = Na.longValue() + Nb.longValue();
//			} else {
//				result = Na.doubleValue() + Nb.doubleValue();
//			}
//			
//			return new JExpression(value(result));
//		}
//		throw new RuntimeException("not supported relations "+a+" & "+b);
//	}
//
//	public static AnimObject sub(List<Relationship> elements) {
//        return new AnimObject(SUB._, elements);
//	}
//
//	public static AnimObject sub(Relationship... elements) {
//        return new AnimObject(SUB._, elements);
//	}
//
//	public static AnimObject mul(List<Relationship> elements) {
//        return new AnimObject(MUL._, elements);
//	}
//
//	public static AnimObject mul(Relationship... elements) {
//        return new AnimObject(MUL._, elements);
//	}
//
//	public static AnimObject div(List<Relationship> elements) {
//        return new AnimObject(DIV._, elements);
//	}
//
//	public static AnimObject div(Relationship... elements) {
//        return new AnimObject(DIV._, elements);
//	}

    @Override
    public void build() throws Throwable {
        builder.start(AN._);
            builder._(REF._, op);
            for (Relationship r : elements) {
                builder.bind(r);
            }
        builder.end();
    }

}