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
import org.animotron.expression.JExpression;
import org.animotron.graph.serializer.CachedSerializer;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;

import static org.animotron.expression.JExpression._;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class HaveLoopTest extends ATest {
	
	@Test
	public void storeAndSerializeResult() throws Exception {
		
		Relationship A = new JExpression(_(THE._, "A", _(AN._, "C")));
		Relationship B = new JExpression(_(THE._, "B", _(AN._, "A")));
		Relationship C = new JExpression(_(THE._, "C", _(AN._, "B")));

		System.out.println("Prepare");
		System.out.print("The a: ");
		CachedSerializer.ANIMO.serialize(A, System.out);
		System.out.println();
		System.out.print("The b: ");
		CachedSerializer.ANIMO.serialize(B, System.out);
		System.out.println();
		System.out.print("The c: ");
		CachedSerializer.ANIMO.serialize(C, System.out);
		System.out.println();
		System.out.println();

        System.out.println("done.");
	}
}