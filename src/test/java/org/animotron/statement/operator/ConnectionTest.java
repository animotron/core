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
package org.animotron.statement.operator;

import org.animotron.ATest;
import org.animotron.expression.JExpression;
import org.animotron.statement.compare.WITH;
import org.animotron.statement.query.ANY;
import org.animotron.statement.query.GET;
import org.animotron.statement.string.AfterLast;
import org.junit.Ignore;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.__;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ConnectionTest extends ATest {
	
    @Test
    public void mimeType_usecase() throws Exception {
        
        __(

            new JExpression(_(THE._, "mime-type")),

            new JExpression(
                _(THE._, "file",
                    _(AN._, "reference", value("file")),
                    _(AN._, "path"),

                    _(AN._, "extension",
                        _(AfterLast._,
                            value("."),
                            _(GET._, "path"))),

                    _(ANY._, "mime-type",
                        _(WITH._, "extension",
                    		_(GET._, "extension")))
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

        JExpression test;
        
//        test = new JExpression(
//            _(GET._, "reference",
//                _(AN._, "fileA")
//        ));
//        assertAnimoResult(test, "reference \"file\".");
//
//        test = new JExpression(
//            _(GET._, "path",
//                _(AN._, "fileA")
//        ));
//        assertAnimoResult(test, "path \"/home/test.txt\".");
//
//        test = new JExpression(
//            _(GET._, "extension",
//                _(AN._, "fileA")
//        ));
//        assertAnimoResult(test, "extension \"txt\".");

        test = new JExpression(
            _(GET._, "mime-type",
                _(AN._, "fileA")
        ));
        //XXX: assertAnimoResult(D, "the D mime-type the value-plain (mime-type) (type \"value/plain\") (extension \"txt\" \"value\").");
        //assertAnimoResult(test, "mime-type the value-plain (mime-type) (type) (extension).");
        assertAnimoResult(test, "the value-plain (mime-type) (type) (extension).");

        test = new JExpression(
            _(GET._, "type",
                _(GET._, "mime-type",
                    _(AN._, "fileA")
        )));
        assertAnimoResult(test, "type \"value/plain\".");
    }
	
    @Test
    public void mimeType_one_more_usecase() throws Exception {

        __(

            new JExpression(_(THE._, "mime-type")),

            new JExpression(
                _(THE._, "file",
                    _(AN._, "reference", value("file")),
                    _(AN._, "path1", value("some.path.value")),

                    _(AN._, "path2",
                        _(GET._, "path1")),

                    _(AN._, "extension1",
                        _(AfterLast._,
                            value("."),
                            _(GET._, "path1"))),

                    _(ANY._, "mime-type",
                        _(WITH._, "extension",
                            _(GET._, "extension1")))
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
        JExpression test;

        test = new JExpression(
            _(GET._, "path1",
                _(AN._, "fileA")
        ));
        assertAnimoResult(test, "path1 \"some.path.value\".");

        test = new JExpression(
            _(GET._, "path2",
                _(AN._, "fileA")
        ));
        assertAnimoResult(test, "path2 path1 \"some.path.value\".");

        test = new JExpression(
            _(GET._, "extension1",
                _(AN._, "fileA")
        ));
        assertAnimoResult(test, "extension1 \"value\".");

        test = new JExpression(
            _(GET._, "mime-type",
                _(AN._, "fileA")
        ));
        //XXX: assertAnimoResult(test, "the value-plain (mime-type) (type \"value/plain\") (extension \"txt\" \"value\").");
        assertAnimoResult(test, "the value-plain (mime-type) (type) (extension).");

        test = new JExpression(
            _(GET._, "type",
	            _(GET._, "mime-type",
	                _(AN._, "fileA")
	            )
            )
        );
        assertAnimoResult(test, "type \"value/plain\".");
    }

    @Test
    @Ignore
    public void mimeType_parallel() throws Exception {

        __(
            new JExpression(_(THE._, "mime-type")),

            new JExpression(
                _(THE._, "file",
                    _(AN._, "reference", value("file")),
                    _(AN._, "path"),

                    _(AN._, "extension",
                        _(AfterLast._,
                            value("."),
                            _(GET._, "path"))),

                    _(ANY._, "mime-type",
                        _(WITH._, "extension",
                            _(GET._, "extension")))
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
        JExpression test;

        test = new JExpression(
    		_(THE._, "test",
		        _(GET._, "mime-type",
		            _(AN._, "fileA")
		        ),
		        _(GET._, "type")
	        )
        );
        assertAnimoResult(test, "the test (mime-type the value-plain (mime-type) (type) (extension)) (type \"value/plain\").");
    }
}