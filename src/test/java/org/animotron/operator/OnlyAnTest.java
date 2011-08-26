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
package org.animotron.operator;

import static org.animotron.Expression._;

import java.io.IOException;

import org.animotron.ATest;
import org.animotron.Expression;
import org.animotron.exception.AnimoException;
import org.animotron.operator.query.GET;
import org.animotron.operator.relation.IS;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class OnlyAnTest extends ATest {

	@Test
	public void test() throws AnimoException, IOException {
        new Expression(
            _(THE._, "red",
                _(IS._, "color")
            )
        );

        new Expression(
            _(THE._, "dress",
                _(AN._, "red")
            )
        );

        Expression a = new Expression(
            _(THE._, "a",
                _(GET._, "color", _(AN._, "dress"))
            )
        );
	
        assertAnimo(a,  "<the:red><is:color/></the:red>");
	}
}