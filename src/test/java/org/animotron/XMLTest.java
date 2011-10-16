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
import org.animotron.expression.StAXExpression;
import org.animotron.graph.serializer.AnimoSerializer;
import org.junit.Ignore;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;

import javax.xml.stream.XMLInputFactory;
import java.io.StringReader;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class XMLTest extends ATest {

    private static final XMLInputFactory FACTORY = new WstxInputFactory();

    private void test(String in, String out) throws Exception {
        Relationship r = new StAXExpression(FACTORY.createXMLStreamReader(new StringReader(in)));
        Assert.assertEquals(out, AnimoSerializer.serialize(r));
    }

    @Test
	public void test_00() throws Exception {
        test("<a/>", "\\a");
	}

    @Test
	public void test_01() throws Exception {
        test("<x:a xmlns:x=\"x-namespace\"/>", "\\x:a $x \"x-namespace\"");
	}

    @Test
	public void test_02() throws Exception {
        test("<a xmlns=\"x-namespace\"/>", "\\a $ \"x-namespace\"");
	}

    @Test
	public void test_03() throws Exception {
        test("<a b=\"c\"/>", "\\a @b \"c\"");
	}

    @Test
    @Ignore
	public void test_04() throws Exception {
        test("<?stylesheet path?><a/>", "??stylesheet \"path\" \\a");
	}

    @Test
	public void test_05() throws Exception {
        test("<a><b><x/></b><c><x/></c></a>", "\\a (\\b \\x) (\\c \\x)");
	}

    @Test
	public void test_06() throws Exception {
        test("<a><b><x/></b><c><x/><y/></c><x><z/></x></a>", "\\a (\\b \\x) (\\c (\\x) (\\y)) (\\x \\z)");
	}

    @Test
	public void test_07() throws Exception {
        test("<a a=\"a\">a</a>", "\\a (@a \"a\") (\"a\")");
	}

    @Test
	public void test_08() throws Exception {
        test("<a xmlns:a=\"a\" a=\"a\">a</a>", "\\a ($a \"a\") (@a \"a\") (\"a\")");
	}

}
