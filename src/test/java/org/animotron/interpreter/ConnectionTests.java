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
import org.animotron.instruction.string.AfterLast;
import org.animotron.operator.AN;
import org.animotron.operator.IC;
import org.animotron.operator.THE;
import org.animotron.operator.compare.WITH;
import org.animotron.operator.query.ANY;
import org.animotron.operator.query.GET;
import org.animotron.operator.relation.HAVE;
import org.animotron.operator.relation.IS;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ConnectionTests extends ATest {
	
	@Test
	public void mimeType_usecase() throws IOException, XMLStreamException {
        System.out.println("Mime type use case ...");
        
    	new Expression(
		_(THE._, "mime-type", 
			_(HAVE._, "extension")
		));

    	new Expression(
		_(THE._, "file", 
			_(HAVE._, "name", text("file")),
			_(HAVE._, "path"),

			_(IC._, "extension", 
				_(AfterLast._, 
					text("."),
					_(GET._, "path"))),

			_(IC._, "mime-type", 
				_(ANY._, "mime-type", 
					_(WITH._, "extension"),
						_(GET._, "extension")))
		));


    	new Expression(
		_(THE._, "fileA", 
			_(IS._, "file"), 
			_(HAVE._, "path", text("/home/test.txt"))
		));

    	new Expression(
		_(THE._, "text-plain", 
			_(IS._, "mime-type"), 
			_(HAVE._, "type", text("text/plain")),
			_(HAVE._, "extension", text("txt"), text("text"))
		));

    	Expression A = new Expression(
		_(THE._, "A", 
			_(GET._, "type", 
				_(GET._, "mime-type", 
					_(AN._, "fileA")
		))));

        //System.out.println("");
        //assertString(A, "text/plain");
        assertAnimo(A, "<the:A><have:type>text/plain</have:type></the:A>");

        //System.out.println("done.");
	}
	
}
