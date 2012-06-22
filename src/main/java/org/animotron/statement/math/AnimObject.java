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
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.DEF;
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
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
	
	protected static Relationship ONE = new JExpression(value(Long.valueOf(1)));
	
	protected static AnimObject PLUS_ONE = new AnimObject(SUM._, ONE);
	protected static AnimObject MINUS_ONE = new AnimObject(SUB._, ONE);
	
	PFlow pf = null;
	List<Relationship> elements = null;
    MathInstruction op = null;
    Relationship result = null;

	private AnimObject(MathInstruction op, Relationship r) {
        super(r);
        this.op = op;
        result = r;
	}

    public AnimObject(PFlow pf, MathInstruction op, Relationship r) {
        super(r);
        this.op = op;
        this.pf = pf;
	}
    
	public AnimObject(PFlow pf, MathInstruction op, List<Relationship> elements) {
        super(new FastGraphBuilder());
		this.elements = elements;
        this.op = op;
        this.pf = pf;
	}

	public AnimObject(PFlow pf, MathInstruction op, Relationship... elements) {
        super(new FastGraphBuilder());
        this.elements = new FastList<Relationship>();
        for (Relationship r : elements) {
		    this.elements.add(r);
        }
        this.op = op;
        this.pf = pf;
	}

	private boolean check(List<Relationship> elements, Relationship r) throws IOException {
		if (r == null) return false;
		
		System.out.println("check");
		System.out.println(r.getType()+" "+r);
    	if (r.isType(VALUE._)) {
    		elements.add(r);
    		return true;
    		
    	} else if (r.isType(AN._) || r.isType(DEF._)) {
    		if (!Utils.haveContext(r.getEndNode())) {

    			if (r.isType(DEF._)) {
        			elements.add(r);
    				
    			} else {
//    				System.out.println(r);
//    				return false;
    				Relationship ref = r.getEndNode().getSingleRelationship(REF._, Direction.OUTGOING);
    				elements.add(DEF.getDefR(ref.getEndNode()));
    			}
    			
        		return true;
    		
    		} else {
                try {
                	Relationship ref = r.getEndNode().getSingleRelationship(REF._, Direction.OUTGOING);
    				Object obj = DEF._.reference(ref.getEndNode());
    				if (obj != null && obj instanceof String ) {
    					Statement s = Statements.name((String) obj);
    					if (s instanceof MathInstruction) {
//    						System.out.println(s);
							elements.add(new AnimObject(pf, (MathInstruction)s, r));
							return true;
    					}
    				} else {
    					//XXX: log
//    					System.out.println("WARNING: REF but no name");
    					return false;
    				}
                } catch (Throwable t){}							
    		}
			throw new RuntimeException("Unsupported operator "+r);
    	}
    	return false; 
	}

	protected List<Relationship> getElements(final PFlow pf) throws IOException {
		if (elements == null)
			elements = getElements(pf, super.relationship());
			
		return elements;
	}

	protected List<Relationship> getElements(final PFlow pf, Relationship op) throws IOException {
		Node n;
		if (op.isType(REF._))
			n = op.getStartNode();
		else
			n = op.getEndNode();
		
		return getElements(pf, n);
	}

	protected List<Relationship> getElements(final PFlow pf, Node n) throws IOException {
		List<Relationship> elements = new FastList<Relationship>();

        IndexHits<Relationship> hits = Order._.context(n);
        try {
            for (Relationship r : hits) {
//            	System.out.println(r+" "+r.getType());
            	if (!check(elements, r)) {
            		if (pf == null)
            			throw new RuntimeException("Unsupported operator "+r);
            		
            		Pipe pipe = Evaluator._.execute(pf.getController(), pf.getVector().question(r), null, false);
                	QCAVector v;
                	while ((v = pipe.take()) != null) {
                		System.out.println(v);
                		if (!check(elements, v.getClosest()))
                			throw new RuntimeException("Unsupported operator "+r+" ["+r.getType()+"]");
                	}
            	}
            }
        } finally {
        	hits.close();
        }
		if (elements.size() == 1 && elements.get(0) instanceof AnimObject)
			elements = ((AnimObject)elements.get(0)).getElements(pf);
			
		return elements;
	}
	
	public Relationship relax(final PFlow pf) throws IOException {
		if (result == null)
			result = op.execute(pf, this);
		
		return result;
	}

    @Override
    public void build() throws Throwable {
        builder.start(AN._);
            builder._(REF._, op.name());
            for (Relationship r : elements) {
                if (r.isType(REF._) || r.isType(DEF._)) {
                    builder.start(AN._);
                        builder._(REF._, r.getEndNode());
                    builder.end();
                } else {
                    builder.bind(r);
                }
            }
        builder.end();
    }
    
    public String toString() {
    	StringBuilder sb = new StringBuilder();
    	sb.append(op.name()).append("\n");
    	if (elements != null)
	    	for (Relationship element : elements) {
	    		sb.append(" ").append(element).append("\n");
	    	}
    	else
    		sb.append("[NOTHING]");
    	return sb.toString();
    }
}
