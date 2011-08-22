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
import org.animotron.exception.AnimoException;
import org.animotron.operator.AN;
import org.animotron.operator.THE;
import org.animotron.operator.compare.WITH;
import org.animotron.operator.query.ANY;
import org.animotron.operator.query.GET;
import org.animotron.operator.relation.HAVE;
import org.animotron.operator.relation.IS;
import org.junit.Test;

import java.io.IOException;

import static org.animotron.Expression._;
import static org.animotron.Expression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class CurrentSiteTest extends ATest {

    @Test
    public void test() throws AnimoException, IOException, InterruptedException {

        new Expression (
            _(THE._, "current-site",
                _(ANY._, "site",
                    _(WITH._, "server-name", _(GET._, "host"))
                )
            )
        );

        new Expression (
            _(THE._, "teset-service",
                _(IS._, "service"),
                _(GET._, "server-name"),
                _(GET._, "host")
            )
        );

        new Expression (
            _(THE._, "localhost-site",
                _(IS._, "site"),
                _(HAVE._, "server-name", text("localhost"))
            )
        );

        new Expression(
            _(THE._, "rest",
                _(ANY._, "service",
                    _(AN._, "current-site")
                )
            )
        );

        Expression s = new Expression(
            _(AN._, "rest",
                _(HAVE._, "host", text("localhost"))
            )
        );

        assertAnimo(s,  "<the:d182256aea683765c5c276803ce3e983ac08d14308a3f7df9e739581ace99de3>" +
                            "<the:rest>" +
                                "<the:teset-service>" +
                                    "<is:service/>" +
                                    "<have:server-name>localhost</have:server-name>" +
                                    "<have:host>localhost</have:host>" +
                                "</the:teset-service>" +
                            "</the:rest>" +
                        "</the:d182256aea683765c5c276803ce3e983ac08d14308a3f7df9e739581ace99de3>");

    }

}
