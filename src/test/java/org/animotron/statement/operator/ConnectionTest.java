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
import org.animotron.statement.relation.SHALL;
import org.animotron.statement.string.AfterLast;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ConnectionTest extends ATest {
	
    @Test
    public void mimeType_usecase() throws Exception {
        
        JExpression.__(

                new JExpression(
                        _(THE._, "mime-type"
                    		//,
                            //_(AN._, "extension")
                        )),

                new JExpression(
                        _(THE._, "file",
                                _(AN._, "reference", value("file")),
                                _(AN._, "path"),

                                _(SHALL._, "extension",
                                        _(AfterLast._,
                                                value("."),
                                                _(GET._, "path"))),

                                _(SHALL._, "mime-type",
                                        _(ANY._, "mime-type",
                                                _(WITH._, "extension",
                                                        _(GET._, "extension"))))
                        )),

                new JExpression(
                        _(THE._, "fileA",
                                _(AN._, "file"),
                                _(AN._, "path", value("/home/test.txt"))
                        )),

                new JExpression(
                        _(THE._, "value-plain",
                                _(AN._, "mime-type"),
                                _(AN._, "type", value("value/plain")),
                                _(AN._, "extension", value("txt"), value("value"))
                        ))

        );

        JExpression A = new JExpression(
        _(THE._, "A",
            _(GET._, "reference",
                _(AN._, "fileA")
        )));
        assertAnimoResult(A, "the A reference \"file\".");

        JExpression B = new JExpression(
        _(THE._, "B",
            _(GET._, "path",
                _(AN._, "fileA")
        )));
        assertAnimoResult(B, "the B path \"/home/test.txt\".");

        JExpression C = new JExpression(
        _(THE._, "C",
            _(GET._, "extension",
                _(AN._, "fileA")
        )));
        //XXX: assertAnimoResult(C, "the C extension \"txt\".");
        assertAnimoResult(C, "the C shall extension \"txt\".");

        JExpression D = new JExpression(
        _(THE._, "D",
            _(GET._, "mime-type",
                _(AN._, "fileA")
        )));
        //XXX: assertAnimoResult(D, "the D mime-type the value-plain (mime-type) (type \"value/plain\") (extension \"txt\" \"value\").");
        //assertAnimoResult(D, "the D shall mime-type the value-plain (mime-type) (type \"value/plain\") (extension \"txt\" \"value\").");
        assertAnimoResult(D, "the D shall mime-type the value-plain (mime-type) (type) (extension).");

        JExpression E = new JExpression(
        _(THE._, "E",
            _(GET._, "type",
                _(GET._, "mime-type",
                    _(AN._, "fileA")
        ))));
        assertAnimoResult(E, "the E type \"value/plain\".");
    }
	
    @Test
    public void mimeType_one_more_usecase() throws Exception {

        JExpression.__(

                new JExpression(
                        _(THE._, "mime-type"
//                        		,
//                                _(AN._, "extension")
                        )),

                new JExpression(
                        _(THE._, "file",
                                _(AN._, "reference", value("file")),
                                _(AN._, "path1", value("some.path.value")),

                                _(SHALL._, "path2",
                                        _(GET._, "path1")),

                                _(SHALL._, "extension1",
                                        _(AfterLast._,
                                                value("."),
                                                _(GET._, "path1"))),

                                _(SHALL._, "mime-type",
                                        _(ANY._, "mime-type",
                                                _(WITH._, "extension",
                                                        _(GET._, "extension1"))))
                        )),

                new JExpression(
                        _(THE._, "fileA",
                                _(AN._, "file"),
                                _(AN._, "path", value("/home/test.txt"))
                        )),

                new JExpression(
                        _(THE._, "value-plain",
                                _(AN._, "mime-type"),
                                _(AN._, "type", value("value/plain")),
                                _(AN._, "extension", value("txt"), value("value"))
                        ))

        );

        JExpression B1 = new JExpression(
        _(THE._, "B1",
            _(GET._, "path1",
                _(AN._, "fileA")
        )));
        assertAnimoResult(B1, "the B1 path1 \"some.path.value\".");

        JExpression B2 = new JExpression(
        _(THE._, "B2",
            _(GET._, "path2",
                _(AN._, "fileA")
        )));
        //XXX: assertAnimoResult(B2, "the B2 path2 path1 \"some.path.value\".");
        assertAnimoResult(B2, "the B2 shall path2 path1 \"some.path.value\".");

        JExpression C1 = new JExpression(
        _(THE._, "C1",
            _(GET._, "extension1",
                _(AN._, "fileA")
        )));
        //XXX: assertAnimoResult(C1, "the C1 extension1 \"value\".");
        assertAnimoResult(C1, "the C1 shall extension1 \"value\".");

        JExpression D = new JExpression(
        _(THE._, "D",
            _(GET._, "mime-type",
                _(AN._, "fileA")
        )));
        //XXX: assertAnimoResult(D, "the D mime-type the value-plain (mime-type) (type \"value/plain\") (extension \"txt\" \"value\").");
        //assertAnimoResult(D, "the D shall mime-type the value-plain (mime-type) (type \"value/plain\") (extension \"txt\" \"value\").");
        assertAnimoResult(D, "the D shall mime-type the value-plain (mime-type) (type) (extension).");

        JExpression E = new JExpression(
        _(THE._, "E",
            _(GET._, "mime-type",
                _(AN._, "fileA")
            ),
            _(GET._, "type")
        ));
        //assertAnimoResult(E, "the E type \"value/plain\".");
        assertAnimoResult(E, "the E (shall mime-type the value-plain (mime-type) (type) (extension)) (type \"value/plain\").");
    }
}