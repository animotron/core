/*
 *  Copyright (C) 2011-2013 The Animo Project
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

import org.animotron.graph.index.Order;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.instruction.DetermInstruction;
import org.neo4j.graphdb.Relationship;

import static org.animotron.statement.operator.VALUE.expression;

/**
 * VALUE instruction 'after-last'.
 *
 * Return last chars from input string after last-found defined pattern.
 *
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class AFTER_LAST extends DetermInstruction {

	public static final AFTER_LAST _ = new AFTER_LAST();

	private AFTER_LAST() { super("after-last"); }

	@Override
	public OnQuestion onCalcQuestion() {
		return new Calc();
	}
	
	class Calc extends OnQuestion {
        @Override
        public void act(final PFlow pf) {
            try {
                //UNDERSTAND: if we have more that 2 params, what to do?
                Relationship[] params = Order._.first(3, pf.getOP().getStartNode());
                String pattern = STRING._.eval(pf, params[1]).toString();
                String source = STRING._.eval(pf, params[2]).toString();
                int index = source.lastIndexOf(pattern);
                if (index != -1) {
                    answered(pf, expression(source.substring(index + 1)));
                }
            } catch (Throwable t) {
                pf.sendException(t);
            }
        }
	}

}