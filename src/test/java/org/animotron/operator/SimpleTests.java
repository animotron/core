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
package org.animotron.operator;

import static org.animotron.Expression._;
import static org.animotron.Expression.text;

import java.io.IOException;
import java.io.InputStream;

import javax.xml.stream.XMLStreamException;

import org.animotron.ATest;
import org.animotron.Expression;
import org.animotron.Reader;
import org.animotron.manipulator.Evaluator;
import org.animotron.operator.query.GET;
import org.animotron.operator.relation.HAVE;
import org.junit.Test;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class SimpleTests extends ATest {
	
	@Test
	public void an() throws Exception {
        System.out.println("Test 'an' ...");
        
    	new Expression(
			_(THE._, "A")
		);

    	new Expression(
			_(THE._, "B", _(HAVE._, "A", text("a@b")))
		);

    	Expression C = new Expression(
			_(THE._, "C", _(AN._, "B"))
		);

        //System.out.println("get:A an:B");
        Evaluator._.execute(C);
        
    	InputStream stream = Reader.read(C);
        assertEquals(stream, "<the:C><the:A></the:A></the:C>");
        
        sleep(10);
        
        //System.out.println("done.");
	}

	private void sleep(int sec) {
		try {
			Thread.sleep(sec * 1000);
		} catch (InterruptedException e) {
		}
	}

	@Test
	public void get() throws IOException, XMLStreamException {
        System.out.println("Test 'get' ...");
        
    	new Expression(
			_(THE._, "A")
		);

    	new Expression(
			_(THE._, "B", _(HAVE._, "A", text("a@b")))
		);

    	Expression C = new Expression(
			_(THE._, "C", _(GET._, "A", _(AN._, "B")))
		);

        //System.out.println("get:A an:B");
        toConsole(Evaluator._.mark(C));
        
    	InputStream stream = Reader.read(C);
        assertEquals(stream, "<the:C><have:A>a@b</have:A></the:C>");
        
        //System.out.println("done.");
	}
}