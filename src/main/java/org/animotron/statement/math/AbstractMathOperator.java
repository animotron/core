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
package org.animotron.statement.math;

import org.animotron.graph.serializer.StringResultSerializer;
import org.animotron.manipulator.ACQVector;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.instruction.Instruction;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.value.AbstractValue;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;

/**
 *
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public abstract class AbstractMathOperator extends Instruction implements Evaluable{

	protected AbstractMathOperator(String name) { super(name); }

    protected Number param (PFlow pf, ACQVector r) throws IOException {
		return AbstractValue.number(StringResultSerializer._.serialize(pf, r.getAnswer()));
    };

    protected Number param (PFlow pf, Relationship r) throws IOException {
		return AbstractValue.number(StringResultSerializer._.serialize(pf, r));
    };

}
