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
package org.animotron.games.web;

import org.animotron.ATest;
import org.animotron.expression.JExpression;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.animotron.statement.query.ALL;
import org.animotron.statement.query.ANY;
import org.animotron.statement.relation.USE;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ResourceTest extends ATest {

    @Test
    public void test1() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "service",
                                _(AN._, "resource")
                        )
                ),
                new JExpression(
                        _(THE._, "root-service",
                                _(AN._, "service"),
                                _(AN._, "root"),
                                _(AN._, "title", text("root"))
                        )
                ),
                new JExpression(
                        _(THE._, "not-found-service",
                                _(AN._, "service"),
                                _(AN._, "not-found"),
                                _(AN._, "title", text("404"))
                        )
                ),
                new JExpression(
                        _(THE._, "rest",
                                _(ANY._, "resource")
                        )
                )
        );

        JExpression s = new JExpression(
            _(THE._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        //assertAnimoResult(s, "the s rest the root-service (service resource) (root) (title \"root\").");
        assertAnimoResult(s, "the s rest the root-service (service resource) (root) (title).");
    }

    @Test
    public void test2() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "service",
                                _(AN._, "resource")
                        )
                ),
                new JExpression(
                        _(THE._, "root-service",
                                _(AN._, "service"),
                                _(AN._, "root"),
                                _(AN._, "title", text("root"))
                        )
                ),
                new JExpression(
                        _(THE._, "root-service1",
                                _(AN._, "service"),
                                _(AN._, "root"),
                                _(AN._, "title", text("root1"))
                        )
                ),
                new JExpression(
                        _(THE._, "not-found-service",
                                _(AN._, "service"),
                                _(AN._, "not-found"),
                                _(AN._, "title", text("404"))
                        )
                ),
                new JExpression(
                        _(THE._, "rest",
                                _(ALL._, "resource")
                        )
                )
        );

        JExpression s = new JExpression(
            _(THE._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        //assertAnimoResult(s, "the s rest (the root-service (service resource) (root) (title \"root\")) (the root-service1 (service resource) (root) (title \"root1\")).");
        assertAnimoResult(s, "the s rest (the root-service (service resource) (root) (title)) (the root-service1 (service resource) (root) (title)).");

    }

    @Test
    public void test3() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "service",
                                _(AN._, "resource")
                        )
                ),
                new JExpression(
                        _(THE._, "root-service",
                                _(AN._, "service"),
                                _(AN._, "root"),
                                _(AN._, "title", text("root"))
                        )
                ),
                new JExpression(
                        _(THE._, "root-service1",
                                _(AN._, "root-service"),
                                _(AN._, "title", text("root1"))
                        )
                ),
                new JExpression(
                        _(THE._, "not-found-service",
                                _(AN._, "service"),
                                _(AN._, "not-found"),
                                _(AN._, "title", text("404"))
                        )
                ),
                new JExpression(
                        _(THE._, "rest",
                                _(ALL._, "resource")
                        )
                )
        );

        JExpression s = new JExpression(
            _(THE._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        //assertAnimoResult(s, "the s rest (the root-service (service resource) (root) (title \"root\")) (the root-service1 (root-service (service resource) (root) (title \"root\")) (title)) (title \"root1\")).");
        assertAnimoResult(s, "the s rest (the root-service (service resource) (root) (title)) (the root-service1 (root-service (service resource) (root) (title)) (title)).");

    }

    @Test
    public void test4() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "root-service",
                                _(AN._, "service"),
                                _(AN._, "root"),
                                _(AN._, "title", text("root"))
                        )
                ),
                new JExpression(
                        _(THE._, "not-found-service",
                                _(AN._, "service"),
                                _(AN._, "not-found"),
                                _(AN._, "title", text("404"))
                        )
                ),
                new JExpression(
                        _(THE._, "rest",
                                _(ANY._, "service")
                        )
                )
        );

        JExpression s = new JExpression(
            _(THE._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        //assertAnimoResult(s, "the s rest the root-service (service) (root) (title \"root\").");
        assertAnimoResult(s, "the s rest the root-service (service) (root) (title).");
    }

    @Test
    public void test5() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "root-service",
                                _(AN._, "service"),
                                _(AN._, "root"),
                                _(AN._, "title", text("root"))
                        )
                ),
                new JExpression(
                        _(THE._, "root-service1",
                                _(AN._, "service"),
                                _(AN._, "root"),
                                _(AN._, "title", text("root1"))
                        )
                ),
                new JExpression(
                        _(THE._, "not-found-service",
                                _(AN._, "service"),
                                _(AN._, "not-found"),
                                _(AN._, "title", text("404"))
                        )
                ),
                new JExpression(
                        _(THE._, "rest",
                                _(ALL._, "service")
                        )
                )
        );

        JExpression s = new JExpression(
            _(THE._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        //assertAnimoResult(s, "the s rest (the root-service (service) (root) (title \"root\")) (the root-service1 (service) (root) (title \"root1\")).");
        assertAnimoResult(s, "the s rest (the root-service (service) (root) (title)) (the root-service1 (service) (root) (title)).");
    }

    @Test
    public void test6() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "root-service",
                                _(AN._, "service"),
                                _(AN._, "root"),
                                _(AN._, "title", text("root"))
                        )
                ),
                new JExpression(
                        _(THE._, "root-service1",
                                _(AN._, "root-service"),
                                _(AN._, "title", text("root1"))
                        )
                ),
                new JExpression(
                        _(THE._, "not-found-service",
                                _(AN._, "service"),
                                _(AN._, "not-found"),
                                _(AN._, "title", text("404"))
                        )
                ),
                new JExpression(
                        _(THE._, "rest",
                                _(ALL._, "service")
                        )
                )
        );

        JExpression s = new JExpression(
            _(THE._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        //assertAnimoResult(s, "the s rest (the root-service (service) (root) (title \"root\")) (the root-service1 (root-service) (title \"root1\")).");
        assertAnimoResult(s, "the s rest (the root-service (service) (root) (title)) (the root-service1 (root-service (service) (root) (title)) (title)).");
    }
}