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
package org.animotron.graph.serializer;

import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.junit.Test;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class JSONSerializerTest extends ATest {

    private void test(String in, String out) throws Exception {
        assertJSONResult(new AnimoExpression(in), out);
    }

    @Test
    public void test_00() throws Exception {
        test("\"foo\"", "\"foo\"");
    }

    @Test
    public void test_01() throws Exception {
        test("the a 1", "1");
    }

    @Test
    public void test_02() throws Exception {
        test("\\b", "{\"b\":null}");
    }

    @Test
    public void test_03() throws Exception {
        test("the a 1 2", "[1,2]");
    }

    @Test
    public void test_04() throws Exception {
        test("((\\a) (\\b))", "[{\"a\":null},{\"b\":null}]");
    }

    @Test
    public void test_05() throws Exception {
        test("\\a \\b 1", "{\"a\":{\"b\":1}}");
    }

    @Test
    public void test_06() throws Exception {
        test("\\a (\\b 1) \"true\" (\\c 2)", "{\"a\":[{\"b\":1},true,{\"c\":2}]}");
    }

    @Test
    public void test_07() throws Exception {
        test("\\a (@b 1) \"true\" (\\c 2)", "{\"a\":[{\"b\":1},true,{\"c\":2}]}");
    }

    @Test
    public void test_08() throws Exception {
        test("\\a (\\b 1) \"true\" (@c 2)", "{\"a\":[{\"b\":1},true,{\"c\":2}]}");
    }

    @Test
    public void test_09() throws Exception {
        test("\\a 1 \"true\" (@c 2)", "{\"a\":[1,true,{\"c\":2}]}");
    }

}
