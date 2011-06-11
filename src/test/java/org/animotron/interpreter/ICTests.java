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
package org.animotron.interpreter;

import static org.animotron.Expression.s;
import static org.animotron.Expression.text;

import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import org.animotron.ATest;
import org.animotron.Expression;
import org.animotron.graph.GraphSerializer;
import org.animotron.operator.AN;
import org.animotron.operator.IC;
import org.animotron.operator.THE;
import org.animotron.operator.query.GET;
import org.animotron.operator.relation.IS;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ICTests extends ATest {
	
	private static final Expression THE_A = 
		new Expression(
				s(THE._, "A")
			);
	
	private static final Expression THE_B = 
		new Expression(
				s(THE._, "B", 
					s(IC._, "A", 
						text(".")
					)
				)
			);

	private static final Expression THE_C = 
		new Expression(
				s(THE._, "C", 
					s(IS._, "B")
				)
			);

	private static final Expression THE_D = 
		new Expression(
				s(THE._, "D", 
					s(GET._, "A", 
						s(AN._, "C")
					)
				)
			);


	@Test
	public void testIC() throws IOException, XMLStreamException {
        System.out.println("Test 'IC' ...");
        
        GraphSerializer.serialize(THE_A, System.out);
        System.out.println();
        GraphSerializer.serialize(THE_B, System.out);
        System.out.println();
        GraphSerializer.serialize(THE_C, System.out);
        System.out.println();
        GraphSerializer.serialize(THE_D, System.out);
        
        //System.out.println("get:A an:C");
        assertString("D", ".");
//        assertEquals("D", "<the:D><have:A>.</have:A></the:D>");

        //System.out.println("done.");
	}
	
}
