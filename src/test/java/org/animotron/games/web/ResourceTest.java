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
package org.animotron.games.web;

import org.animotron.ATest;
import org.animotron.expression.JExpression;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.DEF;
import org.animotron.statement.operator.NONSTOP;
import org.animotron.statement.query.ALL;
import org.animotron.statement.query.ANY;
import org.animotron.statement.relation.USE;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ResourceTest extends ATest {

    @Test
    public void test1() throws Throwable {

        JExpression.__(
                new JExpression(
                        _(DEF._, "service",
                                _(NONSTOP._, "resource")
                        )
                ),
                new JExpression(
                        _(DEF._, "root-service",
                                _(AN._, "service"),
                                _(AN._, "root"),
                                _(AN._, "title", value("root"))
                        )
                ),
                new JExpression(
                        _(DEF._, "not-found-service",
                                _(AN._, "service"),
                                _(AN._, "not-found"),
                                _(AN._, "title", value("404"))
                        )
                ),
                new JExpression(
                        _(DEF._, "rest",
                                _(ANY._, "resource")
                        )
                )
        );

        JExpression s = new JExpression(
            _(DEF._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        //assertAnimoResult(s, "def s rest the root-service (service resource) (root) (title \"root\").");
        assertAnimoResult(s, "def s rest def root-service (service resource) (root) (title).");
    }

    @Test
    public void test2() throws Throwable {

        JExpression.__(
                new JExpression(
                        _(DEF._, "service",
                                _(NONSTOP._, "resource")
                        )
                ),
                new JExpression(
                        _(DEF._, "root-service",
                                _(AN._, "service"),
                                _(AN._, "root"),
                                _(AN._, "title", value("root"))
                        )
                ),
                new JExpression(
                        _(DEF._, "root-service1",
                                _(AN._, "service"),
                                _(AN._, "root"),
                                _(AN._, "title", value("root1"))
                        )
                ),
                new JExpression(
                        _(DEF._, "not-found-service",
                                _(AN._, "service"),
                                _(AN._, "not-found"),
                                _(AN._, "title", value("404"))
                        )
                ),
                new JExpression(
                        _(DEF._, "rest",
                                _(ALL._, "resource")
                        )
                )
        );

        JExpression s = new JExpression(
            _(DEF._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        //assertAnimoResult(s, "def s rest (the root-service (service resource) (root) (title \"root\")) (the root-service1 (service resource) (root) (title \"root1\")).");
        assertAnimoResult(s, "def s rest (def root-service (service resource) (root) (title)) (def root-service1 (service resource) (root) (title)).");

    }

    @Test
    public void test3() throws Throwable {

        JExpression.__(
                new JExpression(
                        _(DEF._, "service",
                                _(NONSTOP._, "resource")
                        )
                ),
                new JExpression(
                        _(DEF._, "root-service",
                                _(NONSTOP._, "service"),
                                _(AN._, "root"),
                                _(AN._, "title", value("root"))
                        )
                ),
                new JExpression(
                        _(DEF._, "root-service1",
                                _(AN._, "root-service"),
                                _(AN._, "title", value("root1"))
                        )
                ),
                new JExpression(
                        _(DEF._, "not-found-service",
                                _(AN._, "service"),
                                _(AN._, "not-found"),
                                _(AN._, "title", value("404"))
                        )
                ),
                new JExpression(
                        _(DEF._, "rest",
                                _(ALL._, "resource")
                        )
                )
        );

        JExpression s = new JExpression(
            _(DEF._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        //assertAnimoResult(s, "def s rest (the root-service (service resource) (root) (title \"root\")) (the root-service1 (root-service (service resource) (root) (title \"root\")) (title)) (title \"root1\")).");
        //assertAnimoResult(s, "def s rest (the root-service (service resource) (root) (title)) (the root-service1 (root-service (service resource) (root) (title)) (title)).");
        assertAnimoResult(s, "def s rest " +
        		"(def root-service (service resource) (root) (title)) " +
        		"(def root-service1 (root-service (service resource) (root) (title)) (title)).");

    }

    @Test
    public void test4() throws Throwable {

        JExpression.__(
                new JExpression(
                        _(DEF._, "root-service",
                                _(AN._, "service"),
                                _(AN._, "root"),
                                _(AN._, "title", value("root"))
                        )
                ),
                new JExpression(
                        _(DEF._, "not-found-service",
                                _(AN._, "service"),
                                _(AN._, "not-found"),
                                _(AN._, "title", value("404"))
                        )
                ),
                new JExpression(
                        _(DEF._, "rest",
                                _(ANY._, "service")
                        )
                )
        );

        JExpression s = new JExpression(
            _(DEF._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        //assertAnimoResult(s, "def s rest the root-service (service) (root) (title \"root\").");
        assertAnimoResult(s, "def s rest def root-service (service) (root) (title).");
    }

    @Test
    public void test5() throws Throwable {

        JExpression.__(
                new JExpression(
                        _(DEF._, "root-service",
                                _(AN._, "service"),
                                _(AN._, "root"),
                                _(AN._, "title", value("root"))
                        )
                ),
                new JExpression(
                        _(DEF._, "root-service1",
                                _(AN._, "service"),
                                _(AN._, "root"),
                                _(AN._, "title", value("root1"))
                        )
                ),
                new JExpression(
                        _(DEF._, "not-found-service",
                                _(AN._, "service"),
                                _(AN._, "not-found"),
                                _(AN._, "title", value("404"))
                        )
                ),
                new JExpression(
                        _(DEF._, "rest",
                                _(ALL._, "service")
                        )
                )
        );

        JExpression s = new JExpression(
            _(DEF._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        //assertAnimoResult(s, "def s rest (the root-service (service) (root) (title \"root\")) (the root-service1 (service) (root) (title \"root1\")).");
        assertAnimoResult(s, "def s rest (def root-service (service) (root) (title)) (def root-service1 (service) (root) (title)).");
    }

    @Test
    public void test6() throws Throwable {

        JExpression.__(
                new JExpression(
                        _(DEF._, "root-service",
                                _(NONSTOP._, "service"),
                                _(AN._, "root"),
                                _(AN._, "title", value("root"))
                        )
                ),
                new JExpression(
                        _(DEF._, "root-service1",
                                _(AN._, "root-service"),
                                _(AN._, "title", value("root1"))
                        )
                ),
                new JExpression(
                        _(DEF._, "not-found-service",
                                _(AN._, "service"),
                                _(AN._, "not-found"),
                                _(AN._, "title", value("404"))
                        )
                ),
                new JExpression(
                        _(DEF._, "rest",
                                _(ALL._, "service")
                        )
                )
        );

        JExpression s = new JExpression(
            _(DEF._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        //assertAnimoResult(s, "def s rest (the root-service (service) (root) (title \"root\")) (the root-service1 (root-service) (title \"root1\")).");
        //assertAnimoResult(s, "def s rest (the root-service (service) (root) (title)) (the root-service1 (root-service (service) (root) (title)) (title)).");
        assertAnimoResult(s, "def s rest " +
    		"(def root-service (service) (root) (title)) " +
    		"(def root-service1 (root-service (service) (root) (title)) (title)).");
    }
}