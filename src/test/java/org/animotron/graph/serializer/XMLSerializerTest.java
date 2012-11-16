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

import junit.framework.Assert;
import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.junit.Ignore;
import org.junit.Test;

import java.io.IOException;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class XMLSerializerTest extends ATest {

    private void test(String in, String out) throws Throwable {
        AnimoExpression expression = new AnimoExpression(in);
        assertXMLResult(expression, out);
    }

    @Test
    public void test_00() throws Throwable {
        test("\\b", "<b/>");
        test("def a \\b", "<b/>");
    }

    @Test
    @Ignore
    public void test_01() throws Throwable {
        test("\\ get element-name element-name \"b\"", "<b/>");
        test("def a \\ get element-name element-name \"b\"", "<b/>");
    }

    @Test
    public void test_02() throws Throwable {
        testAnimo("def b \"c\"");
        test("\\ b", "<c/>");
        test("def a \\ b", "<c/>");
    }

    @Test
    public void test_03() throws Throwable {
        testAnimo("def b \"c\"");
        test("\\ an b", "<c/>");
        test("def a \\ an b", "<c/>");
    }

    @Test
	public void test_04() throws Throwable {
        test("\\ \"b\"", "<b/>");
        test("def a \\ \"b\"", "<b/>");
	}

    @Test
	public void test_05() throws Throwable {
        test("\\b @c \"d\"", "<b c=\"d\"/>");
        test("def a \\b @c \"d\"", "<b c=\"d\"/>");
	}

    @Test
    public void test_06() throws Throwable {
        test("\\b (@c \"d\") (\"e\")", "<b c=\"d\">e</b>");
        test("def a \\b (@c \"d\") (\"e\")", "<b c=\"d\">e</b>");
    }

    @Test
    public void test_07() throws Throwable {
        testAnimo("def b \"b\"");
        testAnimo("def c \"c\"");
        testAnimo("def d \"d\"");
        testAnimo("def e \"e\"");
        test("\\ (b) (@ (c) (d)) (e)", "<b c=\"d\">e</b>");
        test("def a \\ (b) (@ (c) (d)) (e)", "<b c=\"d\">e</b>");
    }

    @Test
    public void test_08() throws Throwable {
        testAnimo("def b \"b\"");
        testAnimo("def c \"c\"");
        testAnimo("def d \"d\"");
        testAnimo("def e \"e\"");
        test("\\((b) (@ (c) (d)) (e))", "<bcde/>");
        test("def a \\((b) (@ (c) (d)) (e))", "<bcde/>");
    }

    @Test
    public void test_09() throws Throwable {
        testAnimo("def b \"b\"");
        testAnimo("def c \"c\"");
        testAnimo("def d \"d\"");
        testAnimo("def e \"e\"");
        testAnimo("def e \\e (b) (c) (d)");
        test("\\(b) (@ (c) (d)) (e)", "<b c=\"d\"><e>bcd</e></b>");
        test("def a \\(b) (@ (c) (d)) (e)", "<b c=\"d\"><e>bcd</e></b>");
    }

    @Test
    public void test_0A() throws Throwable {
        test("def a (??stylesheet) \\root", "<?stylesheet?><root/>");
    }

    @Test
    public void test_0B() throws Throwable {
        test("def a (??stylesheet \"path\") \\root", "<?stylesheet path?><root/>");
    }

    @Test
    public void test_0C() throws Throwable {
        testAnimo("def b \"path\"");
        test("def a (??stylesheet b) \\root", "<?stylesheet path?><root/>");
    }

    @Test
    public void test_0D() throws Throwable {
        test("def a \\x:root $x \"x-namespace\"", "<x:root xmlns:x=\"x-namespace\"/>");
    }

    @Test
    public void test_0E() throws Throwable {
        test("def a \\x:root $ (\"x\") (\"x-namespace\")", "<x:root xmlns:x=\"x-namespace\"/>");
    }

    @Test
    public void test_0F() throws Throwable {
    	IOException ex = null;
        try {
            test("def a \\x:root $x", "<x:root xmlns:x=\"\"/>");
        } catch (IOException e) {
        	ex = e;
        }
        Assert.assertNotNull(ex);
    }

    @Test
    public void test_10() throws Throwable {
        test("def a \\root $ \"x-namespace\"", "<root xmlns=\"x-namespace\"/>");
    }

    @Test
    public void test_11() throws Throwable {
        //TODO implement startDTD()/endDTD()
        test("def a (!! \"<!DOCTYPE html>\") (\\html)", "<!DOCTYPE html><html/>");
    }

    @Test
    public void test_12() throws Throwable {
        test("def a \\html &#amp", "<html>&amp;</html>");
    }

    @Test
    public void test_13() throws Throwable {
        test("def a \\html @x \"&\"", "<html x=\"&amp;\"/>");
    }

    @Test
    public void test_14() throws Throwable {
        testAnimo("def a (b) (\\html).");
        assertXMLResult(new AnimoExpression("any b"), "<html/>");
    }

    @Test
    public void test_15() throws Throwable {
        testAnimo("def a b \\html.");
        assertXMLResult(new AnimoExpression("get b a"), "<html/>");
    }

}
