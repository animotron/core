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
package org.animotron;

import com.ctc.wstx.stax.WstxInputFactory;
import junit.framework.Assert;
import org.animotron.expression.StAXExpression;
import org.animotron.graph.serializer.CachedSerializer;
import org.animotron.graph.serializer.DigestSerializer;
import org.junit.Test;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Relationship;

import javax.xml.stream.XMLInputFactory;
import java.io.StringReader;
import java.util.Iterator;

import static org.animotron.graph.Properties.HASH;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class XMLTest extends ATest {

    private static final XMLInputFactory FACTORY = new WstxInputFactory();

    private String truncate(String s){
        return s.substring(0, s.length() - 1);
    }

    private void test(String in, String out) throws Throwable {
        Relationship r = new StAXExpression(FACTORY.createXMLStreamReader(new StringReader(in)));
        assertEquals((byte[]) HASH.get(r), DigestSerializer._.serialize(r));
        StringBuilder s = new StringBuilder();
        Iterator<Relationship> it = r.getEndNode().getRelationships(Direction.OUTGOING).iterator();
        if (it.hasNext()) {
            Relationship i = it.next();
            s.append(truncate(CachedSerializer.ANIMO.serialize(i)));
            while (it.hasNext()) {
                i = it.next();
                s.append(" ");
                s.append(truncate(CachedSerializer.ANIMO.serialize(i)));
            }
            s.append(".");
        }
        Assert.assertEquals(out, s.toString());
    }

    @Test
	public void test_00() throws Throwable {
        test("<a/>", "\\a.");
	}

    @Test
	public void test_01() throws Throwable {
        test("<x:a xmlns:x=\"x-namespace\"/>", "\\x:a $x \"x-namespace\".");
	}

    @Test
	public void test_02() throws Throwable {
        test("<a xmlns=\"x-namespace\"/>", "\\a $ \"x-namespace\".");
	}

    @Test
	public void test_03() throws Throwable {
        test("<a b=\"c\"/>", "\\a @b \"c\".");
	}

    @Test
	public void test_04() throws Throwable {
        test("<?stylesheet path?><a/>", "??stylesheet \"path\" \\a.");
	}

    @Test
	public void test_05() throws Throwable {
        test("<a><b><x/></b><c><x/></c></a>", "\\a (\\b \\x) (\\c \\x).");
	}

    @Test
	public void test_06() throws Throwable {
        test("<a><b><x/></b><c><x/><y/></c><x><z/></x></a>", "\\a (\\b \\x) (\\c (\\x) (\\y)) (\\x \\z).");
	}

    @Test
	public void test_07() throws Throwable {
        test("<a a=\"a\">a</a>", "\\a (@a \"a\") \"a\".");
	}

    @Test
	public void test_08() throws Throwable {
        test("<a xmlns:a=\"a\" a=\"a\">a</a>", "\\a ($a \"a\") (@a \"a\") \"a\".");
	}

    @Test
	public void test_09() throws Throwable {
        test("<a><!--a--></a>", "\\a !-- \"a\".");
	}

}
