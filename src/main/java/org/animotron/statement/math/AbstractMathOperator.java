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

import org.animotron.graph.serializer.CachedSerializer;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.instruction.DetermInstruction;
import org.animotron.statement.value.AbstractValue;

import java.io.IOException;

/**
 *
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public abstract class AbstractMathOperator extends DetermInstruction {

	protected AbstractMathOperator(String... name) { super(name); }

    protected Number param (QCAVector vector) throws IOException {
    	String number = CachedSerializer.STRING.serialize(vector);
    	
    	if (number.isEmpty()) return null;
    	
		return AbstractValue.number(number);
    };
}
