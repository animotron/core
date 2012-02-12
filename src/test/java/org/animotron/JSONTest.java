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

import junit.framework.Assert;
import org.animotron.expression.Expression;
import org.animotron.expression.JSONExpression;
import org.animotron.graph.serializer.CachedSerializer;
import org.codehaus.jackson.JsonFactory;
import org.codehaus.jackson.JsonParser;
import org.junit.Test;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Relationship;

import java.util.Iterator;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class JSONTest extends ATest {

    private static final JsonFactory FACTORY = new JsonFactory();

    private String truncate(String s){
        return s.substring(0, s.length() - 1);
    }

    private void test(String in, String out) throws Exception {
        JsonParser jp = FACTORY.createJsonParser(in);
        Expression e = new JSONExpression(jp);
        StringBuilder s = new StringBuilder();
        Iterator<Relationship> it = e.getEndNode().getRelationships(Direction.OUTGOING).iterator();
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
    public void test_00() throws Exception {
        test("\"foo\"", "\"foo\".");
    }

    @Test
    public void test_01() throws Exception {
        test("[\"foo\"]", "\"foo\".");
    }

    @Test
    public void test_02() throws Exception {
        test("[\"foo\", \"bar\"]", "\"foo\" \"bar\".");
    }

    @Test
    public void test_03() throws Exception {
        test("{\"foo\" : 1}", "foo 1.");
    }

    @Test
    public void test_04() throws Exception {
        test("{\"foo\" : [1, 2]}", "foo 1 2.");
    }

    @Test
    public void test_05() throws Exception {
        test("{\"foo\" : 1, \"bar\" : 2}", "foo 1 bar 2.");
    }

    @Test
    public void test_06() throws Exception {
        test("{\"foo\" : {\"bar\" : 2} }", "foo bar 2.");
    }

    @Test
    public void test_07() throws Exception {
        test("{\"foo\" : [{\"bar\" : 2}, 3]}", "foo (bar 2) 3.");
    }

    @Test
    public void test_08() throws Exception {
        test("[1, {\"foo\" : [{\"bar\" : 2}, 3]}]", "1 foo (bar 2) 3.");
    }

    @Test
    public void test_09() throws Exception {
        test("{\"foo\" : null, \"bar\" : null}", "foo bar.");
    }

    @Test
    public void test_10() throws Exception {
        test("{\"foo\" : null, \"bar\" : null, \"x\" : null}", "foo bar x.");
    }

}
