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
package org.animotron.statement.security;

import org.animotron.expression.JExpression;
import org.animotron.graph.index.Order;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.instruction.Instruction;
import org.animotron.statement.operator.Evaluable;
import org.animotron.utils.MessageDigester;
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
public class SHA512 extends Instruction implements Evaluable {

	public static final SHA512 _ = new SHA512();

	private SHA512() { super("SHA-512"); }

	@Override
	public OnQuestion onCalcQuestion() {
		return new Calc();
	}
	
	class Calc extends OnQuestion {
        @Override
        public void act(final PFlow pf) {

            Relationship[] params = Order.first(1, pf.getOP().getStartNode());

            //source
            String message;
			try {
				message = STRING.serialize(pf.getVector().question2(params[0]));
			} catch (IOException e) {
				pf.sendException(e);
				return;
			}
			
			message = MessageDigester.calculate(message, "SHA-512");
            
            Relationship r;
			try {
				r = new JExpression(
				    value(
				    		message
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