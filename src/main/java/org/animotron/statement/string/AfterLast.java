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
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.instruction.Instruction;
import org.animotron.statement.operator.Evaluable;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;

import static org.animotron.expression.JExpression.value;
import static org.animotron.graph.serializer.CachedSerializer.STRING;

/**
 * VALUE instruction 'after-last'.
 *
 * Return last chars from input string after last-found defined pattern.
 *
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class AfterLast extends Instruction implements Evaluable {

	public static final AfterLast _ = new AfterLast();

	private AfterLast() { super("after-last"); }

	@Override
	public OnQuestion onCalcQuestion() {
		return new Calc();
	}
	
	class Calc extends OnQuestion {
        @Override
        public void act(final PFlow pf) {

            //UNDERSTAND: if we have more that 2 params, what to do?

            Relationship[] params = Order.first(3, pf.getOP().getStartNode());

            //pattern
            String pattern;
			try {
				pattern = STRING.serialize(params[1]);
			} catch (IOException e) {
				pf.sendException(e);
				return;
			}
            String source;
			try {
				source = STRING.serialize(pf.getVector().question2(params[2]));
			} catch (Exception e) {
				pf.sendException(e);
				return;
			}
            
            int index = source.lastIndexOf(pattern);
            if (index != -1) {

	            Relationship r;
				try {
					r = new JExpression(
					    value(
                                source.substring(index + 1)
                        )
					);
				} catch (Exception e) {
					pf.sendException(e);
					return;
				}
				
				answered(pf, r);
            }
        }
	}
}