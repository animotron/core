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
import org.animotron.statement.instruction.NonDetermInstruction;
import org.animotron.statement.string.STRING;
import org.neo4j.graphdb.Relationship;

import static org.animotron.expression.JExpression.value;

/**
 * Security instruction 'uuid'.
 *
 * Return the uuid.
 *
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class UUID extends NonDetermInstruction {

	public static final UUID _ = new UUID();

	private UUID() { super("uuid"); }

	@Override
	public OnQuestion onCalcQuestion() {
		return new Calc();
	}
	
	class Calc extends OnQuestion {
        @Override
        public void act(final PFlow pf) {
			try {
                Relationship[] params = Order._.first(1, pf.getOP().getStartNode());
                java.util.UUID uuid = params.length > 1
                        ? java.util.UUID.fromString(STRING._.eval(pf, params).toString())
                        : java.util.UUID.randomUUID();
                answered(pf, new JExpression(value(uuid.toString())));
			} catch (Exception e) {
				pf.sendException(e);
			}
        }
	}

}