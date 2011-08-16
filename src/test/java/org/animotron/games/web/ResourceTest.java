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
import org.animotron.Expression;
import org.animotron.exception.EBuilderTerminated;
import org.animotron.operator.AN;
import org.animotron.operator.THE;
import org.animotron.operator.query.ALL;
import org.animotron.operator.query.ANY;
import org.animotron.operator.relation.HAVE;
import org.animotron.operator.relation.IS;
import org.animotron.operator.relation.USE;
import org.junit.Test;

import java.io.IOException;

import static org.animotron.Expression._;
import static org.animotron.Expression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ResourceTest extends ATest {

    @Test
    public void test1() throws EBuilderTerminated, IOException, InterruptedException {

        new Expression(
            _(THE._, "service",
                _(IS._, "resource")
            )
        );

        new Expression(
            _(THE._, "root-service",
                _(IS._, "service"),
                _(IS._, "root"),
                _(HAVE._, "title", text("root"))
            )
        );

        new Expression(
            _(THE._, "not-found-service",
                _(IS._, "service"),
                _(IS._, "not-found"),
                _(HAVE._, "title", text("404"))
            )
        );

        new Expression(
            _(THE._, "rest",
                _(ANY._, "resource")
            )
        );

        Expression s = new Expression(
            _(THE._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        assertAnimo(s,  "<the:s><the:rest><the:root-service><is:service/><is:root/><have:title>root</have:title></the:root-service></the:rest></the:s>");

    }

    @Test
    public void test2() throws EBuilderTerminated, IOException, InterruptedException {

        new Expression(
            _(THE._, "service",
                _(IS._, "resource")
            )
        );

        new Expression(
            _(THE._, "root-service",
                _(IS._, "service"),
                _(IS._, "root"),
                _(HAVE._, "title", text("root"))
            )
        );

        new Expression(
            _(THE._, "root-service1",
                _(IS._, "service"),
                _(IS._, "root"),
                _(HAVE._, "title", text("root1"))
            )
        );

        new Expression(
            _(THE._, "not-found-service",
                _(IS._, "service"),
                _(IS._, "not-found"),
                _(HAVE._, "title", text("404"))
            )
        );

        new Expression(
            _(THE._, "rest",
                _(ALL._, "resource")
            )
        );

        Expression s = new Expression(
            _(THE._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        assertAnimo(s,  "<the:s><the:rest><the:root-service><is:service/><is:root/><have:title>root</have:title></the:root-service><the:root-service1><is:service/><is:root/><have:title>root1</have:title></the:root-service1></the:rest></the:s>");

    }

    @Test
    public void test3() throws EBuilderTerminated, IOException, InterruptedException {

        new Expression(
            _(THE._, "service",
                _(IS._, "resource")
            )
        );

        new Expression(
            _(THE._, "root-service",
                _(IS._, "service"),
                _(IS._, "root"),
                _(HAVE._, "title", text("root"))
            )
        );

        new Expression(
            _(THE._, "root-service1",
                _(IS._, "root-service"),
                _(HAVE._, "title", text("root1"))
            )
        );

        new Expression(
            _(THE._, "not-found-service",
                _(IS._, "service"),
                _(IS._, "not-found"),
                _(HAVE._, "title", text("404"))
            )
        );

        new Expression(
            _(THE._, "rest",
                _(ALL._, "resource")
            )
        );

        Expression s = new Expression(
            _(THE._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        assertAnimo(s,  "<the:s><the:rest><the:root-service><is:service/><is:root/><have:title>root</have:title></the:root-service><the:root-service1><is:root-service/><have:title>root1</have:title></the:root-service1></the:rest></the:s>");

    }

    @Test
    public void test4() throws EBuilderTerminated, IOException, InterruptedException {

        new Expression(
            _(THE._, "root-service",
                _(IS._, "service"),
                _(IS._, "root"),
                _(HAVE._, "title", text("root"))
            )
        );

        new Expression(
            _(THE._, "not-found-service",
                _(IS._, "service"),
                _(IS._, "not-found"),
                _(HAVE._, "title", text("404"))
            )
        );

        new Expression(
            _(THE._, "rest",
                _(ANY._, "service")
            )
        );

        Expression s = new Expression(
            _(THE._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        assertAnimo(s,  "<the:s><the:rest><the:root-service><is:service/><is:root/><have:title>root</have:title></the:root-service></the:rest></the:s>");

    }

    @Test
    public void test5() throws EBuilderTerminated, IOException, InterruptedException {

        new Expression(
            _(THE._, "root-service",
                _(IS._, "service"),
                _(IS._, "root"),
                _(HAVE._, "title", text("root"))
            )
        );

        new Expression(
            _(THE._, "root-service1",
                _(IS._, "service"),
                _(IS._, "root"),
                _(HAVE._, "title", text("root1"))
            )
        );

        new Expression(
            _(THE._, "not-found-service",
                _(IS._, "service"),
                _(IS._, "not-found"),
                _(HAVE._, "title", text("404"))
            )
        );

        new Expression(
            _(THE._, "rest",
                _(ALL._, "service")
            )
        );

        Expression s = new Expression(
            _(THE._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        assertAnimo(s,  "<the:s><the:rest><the:root-service><is:service/><is:root/><have:title>root</have:title></the:root-service><the:root-service1><is:service/><is:root/><have:title>root1</have:title></the:root-service1></the:rest></the:s>");
    }

    @Test
    public void test6() throws EBuilderTerminated, IOException, InterruptedException {

        new Expression(
            _(THE._, "root-service",
                _(IS._, "service"),
                _(IS._, "root"),
                _(HAVE._, "title", text("root"))
            )
        );

        new Expression(
            _(THE._, "root-service1",
                _(IS._, "root-service"),
                _(HAVE._, "title", text("root1"))
            )
        );

        new Expression(
            _(THE._, "not-found-service",
                _(IS._, "service"),
                _(IS._, "not-found"),
                _(HAVE._, "title", text("404"))
            )
        );

        new Expression(
            _(THE._, "rest",
                _(ALL._, "service")
            )
        );

        Expression s = new Expression(
            _(THE._, "s",
                _(AN._, "rest",
                    _(USE._, "root")
                )
            )
        );

        assertAnimo(s,  "<the:s><the:rest><the:root-service><is:service/><is:root/><have:title>root</have:title></the:root-service><the:root-service1><is:root-service/><have:title>root1</have:title></the:root-service1></the:rest></the:s>");

    }

}


