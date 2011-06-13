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

import static org.animotron.Expression._;
import static org.animotron.Expression.text;

import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import org.animotron.ATest;
import org.animotron.Expression;
import org.animotron.operator.THE;
import org.animotron.operator.compare.WITH;
import org.animotron.operator.query.ANY;
import org.animotron.operator.relation.HAVE;
import org.animotron.operator.relation.IS;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ANYTests extends ATest {
	

	@Test
	public void testANY() throws IOException, XMLStreamException {
        System.out.println("Test 'ANY' ...");
        
    	new Expression(
			_(THE._, "A", _(HAVE._, "value"))
		);
	
    	new Expression(
			_(THE._, "B", _(IS._, "A"), _(HAVE._, "value", text("B")))
		);

    	new Expression(
			_(THE._, "C", _(IS._, "B"), _(HAVE._, "value", text("C")))
		);

    	new Expression(
			_(THE._, "D", _(ANY._, "A", _(WITH._, "value", text("B"))))
		);

        //System.out.println("any:A have:value B");
        assertEquals("D", "<the:D><the:B><have:value>B</have:value></the:B></the:D>");

        //System.out.println("done.");
	}
	
}
