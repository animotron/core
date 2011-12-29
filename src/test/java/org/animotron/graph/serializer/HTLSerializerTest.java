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
import org.animotron.expression.AnimoExpression;
import org.junit.Test;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class HTLSerializerTest extends ATest {

    private void test(String in, String out) throws Exception {
        AnimoExpression expression = new AnimoExpression(in);
        assertHtmlResult(expression, out);
    }

    @Test
    public void test_00() throws Exception {
        test(
            "(" +
                "(!! \"<!doctype html>\") " +
                "(\\html " +
                    "(\\head " +
                        "(\\title \"Hello World!\") " +
                        "(\\script @src \"/foo/bar.js\") " +
                        "(\\script \"x = a < b; y = c > d; z = e && f;\")) " +
                    "(\\body " +
                        "(\\h1 \"Hello World!\") " +
                        "(\\p \"Welcome to Animo!\") " +
                        "(\\br) "+
                        "(\\input (@name \"test\") (@value \"\"foo\" & 'bar'.\") (@checked)))))",
            "<!DOCTYPE html>" +
            "<html>" +
                "<head>" +
                    "<title>Hello World!</title>" +
                    "<script src=\"/foo/bar.js\"/>" +
                    "<script>" +
                        "x = a < b; y = c > d; z = e && f;" +
                    "</script>" +
                "</head>" +
                "<body>" +
                    "<h1>Hello World!</h1>" +
                    "<p>Welcome to Animo!</p>" +
                    "<br/>" +
                    "<input name=\"test\" value=\"\" checked/>" +
                "</body>" +
            "</html>");
    }

}
