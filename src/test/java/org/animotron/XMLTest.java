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
package org.animotron;

import com.ctc.wstx.stax.WstxInputFactory;
import junit.framework.Assert;
import org.animotron.exception.AnimoException;
import org.animotron.expression.StAXExpression;
import org.animotron.graph.serializer.AnimoSerializer;
import org.junit.Test;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Relationship;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import java.io.IOException;
import java.io.StringReader;
import java.util.Iterator;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class XMLTest extends ATest {

    private static final XMLInputFactory FACTORY = new WstxInputFactory();

    private void test(String in, String out) throws AnimoException, IOException, XMLStreamException {
        Relationship r = new StAXExpression(FACTORY.createXMLStreamReader(new StringReader(in)));
        Iterator<Relationship> it = r.getEndNode().getRelationships(Direction.OUTGOING).iterator();
        StringBuilder s = new StringBuilder();
        AnimoSerializer.serialize(it.next(), s);
        while (it.hasNext()){
            s.append(" ");
            AnimoSerializer.serialize(it.next(), s);
        }
        Assert.assertEquals(out, s.toString());
    }

    @Test
	public void test_00() throws IOException, AnimoException, XMLStreamException {
        test("<a/>", "\\a");
	}

    @Test
	public void test_01() throws IOException, AnimoException, XMLStreamException {
        test("<x:a xmlns:x=\"x-namespace\"/>", "\\x:a $x \"x-namespace\"");
	}

    @Test
	public void test_02() throws IOException, AnimoException, XMLStreamException {
        test("<a xmlns=\"x-namespace\"/>", "\\a $ \"x-namespace\"");
	}

    @Test
	public void test_03() throws IOException, AnimoException, XMLStreamException {
        test("<a b=\"c\"/>", "\\a @b \"c\"");
	}

    @Test
	public void test_04() throws IOException, AnimoException, XMLStreamException {
        test("<?stylesheet path?><a/>", "??stylesheet \"path\" \\a");
	}

}
