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
import org.animotron.expression.JExpression;
import org.animotron.statement.compare.WITH;
import org.animotron.statement.query.ANY;
import org.animotron.statement.query.GET;
import org.animotron.statement.query.SELF;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.animotron.statement.string.AfterLast;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ConnectionTest extends ATest {
	
    @Test
    public void mimeType_usecase() throws Exception {
        
        new JExpression(
        _(THE._, "mime-type",
            _(HAVE._, "extension")
        ));

        new JExpression(
        _(THE._, "file",
            _(HAVE._, "reference", text("file")),
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

        new JExpression(
        _(THE._, "fileA",
            _(IS._, "file"),
            _(HAVE._, "path", text("/home/test.txt"))
        ));

        new JExpression(
        _(THE._, "text-plain",
            _(IS._, "mime-type"),
            _(HAVE._, "type", text("text/plain")),
            _(HAVE._, "extension", text("txt"), text("text"))
        ));

        JExpression A = new JExpression(
        _(THE._, "A",
            _(GET._, "reference",
                _(AN._, "fileA")
        )));
        assertAnimoResult(A, "the A have reference \"file\"");

        JExpression B = new JExpression(
        _(THE._, "B",
            _(GET._, "path",
                _(AN._, "fileA")
        )));
        assertAnimoResult(B, "the B have path \"/home/test.txt\"");

        JExpression C = new JExpression(
        _(THE._, "C",
            _(GET._, "extension",
                _(AN._, "fileA")
        )));
        assertAnimoResult(C, "the C have extension \"txt\"");

        JExpression D = new JExpression(
        _(THE._, "D",
            _(GET._, "mime-type",
                _(AN._, "fileA")
        )));
        assertAnimoResult(D, "the D have mime-type the text-plain (is mime-type) (have type \"text/plain\") (have extension (\"txt\") (\"text\"))");

        JExpression E = new JExpression(
        _(THE._, "E",
            _(GET._, "type",
                _(GET._, "mime-type",
                    _(AN._, "fileA")
        ))));
        assertAnimoResult(E, "the E have type \"text/plain\"");
    }
	
    @Test
    public void mimeType_one_more_usecase() throws Exception {

        new JExpression(
        _(THE._, "mime-type",
            _(HAVE._, "extension")
        ));

        new JExpression(
        _(THE._, "file",
            _(HAVE._, "reference", text("file")),
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

        new JExpression(
        _(THE._, "fileA",
            _(IS._, "file"),
            _(HAVE._, "path", text("/home/test.txt"))
        ));

        new JExpression(
        _(THE._, "text-plain",
            _(IS._, "mime-type"),
            _(HAVE._, "type", text("text/plain")),
            _(HAVE._, "extension", text("txt"), text("text"))
        ));

        JExpression B1 = new JExpression(
        _(THE._, "B1",
            _(GET._, "path1",
                _(AN._, "fileA")
        )));
        assertAnimoResult(B1, "the B1 have path1 \"some.path.text\"");

        JExpression B2 = new JExpression(
        _(THE._, "B2",
            _(GET._, "path2",
                _(AN._, "fileA")
        )));
        assertAnimoResult(B2, "the B2 have path2 have path1 \"some.path.text\"");

        JExpression C1 = new JExpression(
        _(THE._, "C1",
            _(GET._, "extension1",
                _(AN._, "fileA")
        )));
        assertAnimoResult(C1, "the C1 have extension1 \"text\"");

        JExpression D = new JExpression(
        _(THE._, "D",
            _(GET._, "mime-type",
                _(AN._, "fileA")
        )));
        assertAnimoResult(D, "the D have mime-type the text-plain (is mime-type) (have type \"text/plain\") (have extension (\"txt\") (\"text\"))");

        JExpression E = new JExpression(
        _(THE._, "E",
            _(GET._, "type",
                _(GET._, "mime-type",
                    _(AN._, "fileA")
        ))));
        assertAnimoResult(E, "the E have type \"text/plain\"");
    }
}