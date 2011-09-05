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
package org.animotron.statement.operator;

import org.animotron.ATest;
import org.animotron.Expression;
import org.animotron.statement.compare.WITH;
import org.animotron.statement.query.ANY;
import org.animotron.statement.query.GET;
import org.animotron.statement.query.SELF;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.animotron.statement.string.AfterLast;
import org.junit.Test;

import static org.animotron.Expression._;
import static org.animotron.Expression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ConnectionTest extends ATest {
	
    @Test
    public void mimeType_usecase() throws Exception {
        
        new Expression(
        _(THE._, "mime-type",
            _(HAVE._, "extension")
        ));

        new Expression(
        _(THE._, "file",
            _(HAVE._, "name", text("file")),
            _(HAVE._, "path"),

            _(IC._, "extension",
                _(AfterLast._,
                    text("."),
                    _(SELF._, "path"))),

            _(IC._, "mime-type",
                _(ANY._, "mime-type",
                    _(WITH._, "extension",
                        _(SELF._, "extension"))))
        ));

        new Expression(
        _(THE._, "fileA",
            _(IS._, "file"),
            _(HAVE._, "path", text("/home/test.txt"))
        ));

        new Expression(
        _(THE._, "text-plain",
            _(IS._, "mime-type"),
            _(HAVE._, "type", text("text/plain")),
            _(HAVE._, "extension", text("txt"), text("text"))
        ));

        Expression A = new Expression(
        _(THE._, "A",
            _(GET._, "name",
                _(AN._, "fileA")
        )));
        //assertXMLResult(A, "<the:A><have:name>file</have:name></the:A>");
        assertAnimoResult(A, "the A have name \"file\"\n");

        Expression B = new Expression(
        _(THE._, "B",
            _(GET._, "path",
                _(AN._, "fileA")
        )));
        //assertXMLResult(B, "<the:B><have:path>/home/test.txt</have:path></the:B>");
        assertAnimoResult(B, "the B have path \"/home/test.txt\"\n");

        Expression C = new Expression(
        _(THE._, "C",
            _(GET._, "extension",
                _(AN._, "fileA")
        )));
        //assertXMLResult(C, "<the:C><have:extension>txt</have:extension></the:C>");
        assertAnimoResult(C, "the C have extension \"txt\"\n");

        Expression D = new Expression(
        _(THE._, "D",
            _(GET._, "mime-type",
                _(AN._, "fileA")
        )));
        //assertXMLResult(D, "<the:D><have:mime-type><the:text-plain><is:mime-type/><have:type>text/plain</have:type><have:extension>txttext</have:extension></the:text-plain></have:mime-type></the:D>");
        assertAnimoResult(D, "the D have mime-type (the text-plain is mime-type have type \"text/plain\" (have extension \"txt\" \"text\"))\n");

        Expression E = new Expression(
        _(THE._, "E",
            _(GET._, "type",
                _(GET._, "mime-type",
                    _(AN._, "fileA")
        ))));
        //assertXMLResult(E, "<the:E><have:type>text/plain</have:type></the:E>");
        assertAnimoResult(E, "the E have type \"text/plain\"\n");
    }
	
    @Test
    public void mimeType_one_more_usecase() throws Exception {

        new Expression(
        _(THE._, "mime-type",
            _(HAVE._, "extension")
        ));

        new Expression(
        _(THE._, "file",
            _(HAVE._, "name", text("file")),
            _(HAVE._, "path1", text("some.path.text")),

            _(IC._, "path2",
                _(SELF._, "path1")),

            _(IC._, "extension1",
                _(AfterLast._,
                    text("."),
                    _(SELF._, "path1"))),

            _(IC._, "mime-type",
                _(ANY._, "mime-type",
                    _(WITH._, "extension",
                        _(SELF._, "extension1"))))
        ));

        new Expression(
        _(THE._, "fileA",
            _(IS._, "file"),
            _(HAVE._, "path", text("/home/test.txt"))
        ));

        new Expression(
        _(THE._, "text-plain",
            _(IS._, "mime-type"),
            _(HAVE._, "type", text("text/plain")),
            _(HAVE._, "extension", text("txt"), text("text"))
        ));

        Expression B1 = new Expression(
        _(THE._, "B1",
            _(GET._, "path1",
                _(AN._, "fileA")
        )));
        //assertXMLResult(B1, "<the:B1><have:path1>some.path.text</have:path1></the:B1>");
        assertAnimoResult(B1, "the B1 have path1 \"some.path.text\"\n");

        Expression B2 = new Expression(
        _(THE._, "B2",
            _(GET._, "path2",
                _(AN._, "fileA")
        )));
        //assertXMLResult(B2, "<the:B2><have:path2><have:path1>some.path.text</have:path1></have:path2></the:B2>");
        assertAnimoResult(B2, "the B2 have path2 have path1 \"some.path.text\"\n");

        Expression C1 = new Expression(
        _(THE._, "C1",
            _(GET._, "extension1",
                _(AN._, "fileA")
        )));
        //assertXMLResult(C1, "<the:C1><have:extension1>text</have:extension1></the:C1>");
        assertAnimoResult(C1, "the C1 have extension1 \"text\"\n");

        Expression D = new Expression(
        _(THE._, "D",
            _(GET._, "mime-type",
                _(AN._, "fileA")
        )));
        //assertXMLResult(D, "<the:D><have:mime-type><the:text-plain><is:mime-type/><have:type>text/plain</have:type><have:extension>txttext</have:extension></the:text-plain></have:mime-type></the:D>");
        assertAnimoResult(D, "the D have mime-type (the text-plain is mime-type have type \"text/plain\" (have extension \"txt\" \"text\"))\n");

        Expression E = new Expression(
        _(THE._, "E",
            _(GET._, "type",
                _(GET._, "mime-type",
                    _(AN._, "fileA")
        ))));
        //assertXMLResult(E, "<the:E><have:type>text/plain</have:type></the:E>");
        assertAnimoResult(E, "the E have type \"text/plain\"\n");
    }
}