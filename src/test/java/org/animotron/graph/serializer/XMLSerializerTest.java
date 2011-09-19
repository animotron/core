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
package org.animotron.graph.serializer;

import org.animotron.ATest;
import org.animotron.Expression;
import org.animotron.exception.AnimoException;
import org.animotron.graph.builder.AnimoBuilder;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.junit.Test;

import java.io.IOException;

import static org.animotron.Expression.*;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class XMLSerializerTest extends ATest {

    private void test(String in, String out) throws AnimoException, IOException {
        AnimoBuilder builder = new AnimoBuilder(in);
        builder.build();
        assertXMLResult(builder.getRelationship(), out);
    }

    @Test
    public void test_00() throws IOException, AnimoException {
        test("\\b", "<b/>");
        test("the a \\b", "<b/>");
    }

    @Test
    public void test_01() throws IOException, AnimoException {
        test("\\ get element-name have element-name \"b\"", "<b/>");
        test("the a \\ get element-name have element-name \"b\"", "<b/>");
    }

    @Test
    public void test_02() throws IOException, AnimoException {
        new Expression(_(THE._, "b", text("c")));
        test("\\ b", "<c/>");
        test("the a \\ b", "<c/>");
    }

    @Test
    public void test_03() throws IOException, AnimoException {
        new Expression(_(THE._, "b", text("c")));
        test("\\ an b", "<c/>");
        test("the a \\ an b", "<c/>");
    }

    @Test
	public void test_04() throws IOException, AnimoException {
        test("\\ \"b\"", "<b/>");
        test("the a \\ \"b\"", "<b/>");
	}

    @Test
	public void test_05() throws IOException, AnimoException {
        test("\\b @c \"d\"", "<b c=\"d\"/>");
        test("the a \\b @c \"d\"", "<b c=\"d\"/>");
	}

    @Test
    public void test_06() throws IOException, AnimoException {
        test("\\b (@c \"d\") (\"e\")", "<b c=\"d\">e</b>");
        test("the a \\b (@c \"d\") (\"e\")", "<b c=\"d\">e</b>");
    }

    @Test
    public void test_07() throws IOException, AnimoException {
        new Expression(_(THE._, "b", text("b")));
        new Expression(_(THE._, "c", text("c")));
        new Expression(_(THE._, "d", text("d")));
        new Expression(_(THE._, "e", text("e")));
        test("\\ (b) (@ (c) (d)) (e)", "<b c=\"d\">e</b>");
        test("the a \\ (b) (@ (c) (d)) (e)", "<b c=\"d\">e</b>");
    }

    @Test
    public void test_08() throws IOException, AnimoException {
        new Expression(_(THE._, "b", text("b")));
        new Expression(_(THE._, "c", text("c")));
        new Expression(_(THE._, "d", text("d")));
        new Expression(_(THE._, "e", text("e")));
        test("\\((b) (@ (c) (d)) (e))", "<b c=\"d\">e</b>");
        test("the a \\((b) (@ (c) (d)) (e))", "<b c=\"d\">e</b>");
    }

    @Test
    public void test_09() throws IOException, AnimoException {
        new Expression(_(THE._, "b", text("b")));
        new Expression(_(THE._, "c", text("c")));
        new Expression(_(THE._, "d", text("d")));
        new Expression(_(THE._, "e", element("e", _(AN._, "b"), _(AN._, "c"), _(AN._, "d"))));
        test("\\(b) (@ (c) (d)) (e)", "<b c=\"d\"><e>bcd</e></b>");
        test("the a \\(b) (@ (c) (d)) (e)", "<b c=\"d\"><e>bcd</e></b>");
    }

    @Test
    public void test_0A() throws IOException, AnimoException {
        test("the a (??stylesheet) \\root", "<?stylesheet?><root/>");
    }

    @Test
    public void test_0B() throws IOException, AnimoException {
        test("the a (??stylesheet \"path\") \\root", "<?stylesheet path?><root/>");
    }

    @Test
    public void test_0C() throws IOException, AnimoException {
        new Expression(_(THE._, "b", text("path")));
        test("the a (??stylesheet b) \\root", "<?stylesheet path?><root/>");
    }

}
