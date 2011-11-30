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
import org.animotron.statement.query.ANY;
import org.animotron.statement.query.GET;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class YetAnotherHaveLoopTest extends ATest {
	
    @Test
    public void test() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "html",
                                _(AN._, "content",
                                        _(AN._, "root-layout")
                                )
                        )
                ),
                new JExpression(
                        _(THE._, "it-working",
                                _(AN._, "content", text("It is working!"))
                        )
                ),
                new JExpression(
                        _(THE._, "service",
                                _(AN._, "html",
                                        _(AN._, "it-working")
                                )
                        )
                ),
                new JExpression(
                        _(THE._, "root-layout",
                                _(AN._, "layout"),
                                _(GET._, "content")
                        )
                )
        );

        JExpression s = new JExpression(
            _(THE._, "s",
                _(GET._, "content",
                    _(AN._, "service",
                        _(AN._, "uri", text("/")),
                        _(AN._, "host", text("localhost"))
                    )
                )
            )
        );

        assertAnimoResult(s, "the s content the root-layout (layout) (content \"It is working!\").");

   }

    @Test
    public void test1() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "html",
                                _(AN._, "content",
                                        _(ANY._, "layout")
                                )
                        )
                ),
                new JExpression(
                        _(THE._, "it-working",
                                _(AN._, "content", text("It is working!"))
                        )
                ),
                new JExpression(
                        _(THE._, "service",
                                _(AN._, "html",
                                        _(AN._, "it-working")
                                )
                        )
                ),
                new JExpression(
                        _(THE._, "root-layout",
                                _(AN._, "layout"),
                                _(GET._, "content")
                        )
                )
        );

        JExpression s = new JExpression(
            _(THE._, "s",
                _(GET._, "content",
                    _(AN._, "service",
                        _(AN._, "uri", text("/")),
                        _(AN._, "host", text("localhost"))
                    )
                )
            )
        );

        assertAnimoResult(s, "the s content the root-layout (layout) (content \"It is working!\").");
   }
}