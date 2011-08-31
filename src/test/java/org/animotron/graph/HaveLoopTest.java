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
package org.animotron.graph;

import org.animotron.ATest;
import org.animotron.Expression;
import org.animotron.graph.serializer.AnimoSerializer;
import org.animotron.statement.operator.THE;
import org.animotron.statement.relation.HAVE;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;

import static org.animotron.Expression._;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class HaveLoopTest extends ATest {
	
	@Test
	public void storeAndSerializeResult() throws Exception {
		
		Relationship A = new Expression(_(THE._, "A", _(HAVE._, "C")));
		Relationship B = new Expression(_(THE._, "B", _(HAVE._, "A")));
		Relationship C = new Expression(_(THE._, "C", _(HAVE._, "B")));

		System.out.println("Prepare");
		System.out.print("The a: ");
		AnimoSerializer.serialize(A, System.out);
		System.out.println();
		System.out.print("The b: ");
		AnimoSerializer.serialize(B, System.out);
		System.out.println();
		System.out.print("The c: ");
		AnimoSerializer.serialize(C, System.out);
		System.out.println();
		System.out.println();

        System.out.println("done.");
	}
}
