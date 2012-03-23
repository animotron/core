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
public class HTLSerializerTest extends ATest {

    private void test(String in, String out) throws Throwable {
        AnimoExpression expression = new AnimoExpression(in);
        assertHtmlResult(expression, out);
    }

    @Test
    public void test_00() throws Throwable {
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
            "<html>" +
                "<head>" +
                    "<title>Hello World!</title>" +
                    "<script src=\"/foo/bar.js\"></script>" +
                    "<script>" +
                        "x = a < b; y = c > d; z = e && f;" +
                    "</script>" +
                "</head>" +
                "<body>" +
                    "<h1>Hello World!</h1>" +
                    "<p>Welcome to Animo!</p>" +
                    "<br>" +
                    "<input name=\"test\" value=\"\" checked>" +
                "</body>" +
            "</html>");
    }

}
