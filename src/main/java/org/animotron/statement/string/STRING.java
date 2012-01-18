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
package org.animotron.statement.string;

import org.animotron.expression.JExpression;
import org.animotron.graph.index.Order;
import org.animotron.graph.serializer.CachedSerializer;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.instruction.Instruction;
import org.animotron.statement.operator.Evaluable;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;

import static org.animotron.expression.JExpression.value;

/**
 * VALUE instruction 'STRING'.
 *
 * Return last chars from input string after last-found defined pattern.
 *
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class STRING extends Instruction implements Evaluable {

	public static final STRING _ = new STRING();

	private STRING() { super("string"); }


	@Override
	public OnQuestion onCalcQuestion() {
		return new OnQuestion(){
	        @Override
	        public void act(final PFlow pf) {
	
	            //UNDERSTAND: if we have more that 2 params, what to do?
	        	
	        	StringBuilder sb = new StringBuilder(); 
	        	
	            IndexHits<Relationship> hits = Order.context(pf.getOP().getStartNode());
	            try {
	            	for (Relationship r : hits) {
	            		try {
							sb.append(CachedSerializer.STRING.serialize(pf.getVector().question2(r)));
						} catch (IOException e) {
							pf.sendException(e);
							return;
						}
	            	}
	            } finally {
	            	hits.close();
	            }
	            
	            if (sb.length() > 0) {
	
		            Relationship r;
					try {
						r = new JExpression(
						    value(
	                            sb.toString()
	                        )
						);
					} catch (Exception e) {
						pf.sendException(e);
						return;
					}
					answered(pf, r);
	            }
	        }
	    };
	}
}
