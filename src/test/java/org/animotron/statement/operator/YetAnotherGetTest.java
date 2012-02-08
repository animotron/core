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
import org.animotron.statement.query.GET;
import org.animotron.statement.relation.SHALL;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.__;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class YetAnotherGetTest extends ATest{

    @Test
    public void get_via_is() throws Exception {

        __(
            new JExpression(
                    _(THE._, "B", _(AN._, "A"))
            ),
            new JExpression(
                    _(THE._, "C", _(AN._, "B", value("π")))
            )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(GET._, "A", _(AN._, "C")))
        );
        assertAnimoResult(E, "the E B \"π\".");

    }

    @Test
    public void get_shall_via_is() throws Exception {

        __(
            new JExpression(
                    _(THE._, "B", _(AN._, "A"))
            ),
            new JExpression(
                    _(THE._, "C", _(SHALL._, "B", value("π")))
            ),
            new JExpression(
                    _(THE._, "D", _(AN._, "C"))
            )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(GET._, "A", _(AN._, "D")))
        );
        //XXX: assertAnimoResult(E, "the E B \"π\".");
        assertAnimoResult(E, "the E shall B \"π\".");
    }

    @Test
    public void get_have_via_is() throws Exception {

        __(
            new JExpression(
                    _(THE._, "B", _(AN._, "A"))
            ),
            new JExpression(
                    _(THE._, "C", _(AN._, "B", value("π")))
            ),
            new JExpression(
                    _(THE._, "D", _(AN._, "C"))
            )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(GET._, "A", _(AN._, "D")))
        );
        assertAnimoResult(E, "the E B \"π\".");
    }
}