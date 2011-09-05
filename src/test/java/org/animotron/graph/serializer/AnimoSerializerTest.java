/*
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
package org.animotron.graph.serializer;

import org.animotron.ATest;
import org.animotron.Expression;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.animotron.statement.relation.USE;
import org.junit.Test;

import static org.animotron.Expression._;
import static org.animotron.Expression.text;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnimoSerializerTest extends ATest {
	
	@Test
	public void test() throws Exception {

        Expression A;

        A = new Expression(
            _(THE._, "A",
                _(IS._, "X"),
                _(USE._, "Y")
            )
        );
        assertAnimo(A, "the A (is X) (use Y)");

        A = new Expression(
            _(THE._, "A",
                _(HAVE._, "X"),
                _(HAVE._, "Y")
            )
        );
        assertAnimo(A, "the A (have X) (have Y)");

        A = new Expression(
            _(THE._, "A",
                _(HAVE._, "X",
                    _(HAVE._, "Y")
                )
            )
        );
        assertAnimo(A, "the A have X have Y");

        A = new Expression(
            _(THE._, "A",
                _(AN._, "X"),
                _(AN._, "Y")
            )
        );
        assertAnimo(A, "the A (an X) (an Y)");

        A = new Expression(
            _(THE._, "A",
                _(AN._, "X",
                    _(AN._, "Y")
                )
            )
        );
        assertAnimo(A, "the A an X an Y");

        A = new Expression(
            _(THE._, "A",
                _(HAVE._, "B",
                    _(AN._, "C",
                        _(HAVE._, "D", text("."))
                    )
                )
            )
        );
        assertAnimo(A, "the A have B an C have D \".\"");

        A = new Expression(
            _(THE._, "A",
                _(HAVE._, "B",
                    _(AN._, "C",
                        _(HAVE._, "D"),
                        text(".")
                    )
                )
            )
        );
        assertAnimo(A, "the A have B an C (have D) (\".\")");

        A = new Expression(
            _(THE._, "A",
                _(HAVE._, "B",
                    _(AN._, "C",
                        _(HAVE._, "D", text(".")),
                        _(HAVE._, "E", text("_"))
                    )
                )
            )
        );
        assertAnimo(A, "the A have B an C (have D \".\") (have E \"_\")");

        A = new Expression(
            _(THE._, "A",
                _(IS._, "X"),
                _(HAVE._, "B",
                    _(AN._, "C",
                        _(HAVE._, "D", text(".")),
                        _(HAVE._, "E", text("_"))
                    )
                )
            )
        );
        assertAnimo(A, "the A (is X) (have B an C (have D \".\") (have E \"_\"))");

        A = new Expression(
            _(THE._, "A",
                _(IS._, "X"),
                _(HAVE._, "B",
                    _(AN._, "C",
                        _(HAVE._, "D", text(".")),
                        _(HAVE._, "E", text("_"))
                    ),
                    _(AN._, "F",
                        _(HAVE._, "G", text(":")),
                        _(HAVE._, "H", text(";"))
                    )
                )
            )
        );
        assertAnimo(A, "the A (is X) (have B (an C (have D \".\") (have E \"_\")) (an F (have G \":\") (have H \";\")))");

        A = new Expression(
            _(THE._, "A",
                _(IS._, "X"),
                _(HAVE._, "B",
                    _(AN._, "C",
                        _(HAVE._, "D", text("1")),
                        _(HAVE._, "E", text("2"))
                    ),
                    _(AN._, "F",
                        _(HAVE._, "G", text("3")),
                        _(HAVE._, "H", text("4"))
                    )
                ),
                _(HAVE._, "I",
                    _(AN._, "J",
                        _(HAVE._, "K", text("5")),
                        _(HAVE._, "L", text("6"))
                    ),
                    _(AN._, "F",
                        _(HAVE._, "M", text("7")),
                        _(HAVE._, "N", text("8"))
                    )
                )
            )
        );
        assertAnimo(A, "the A (is X) (have B (an C (have D \"1\") (have E \"2\")) (an F (have G \"3\") (have H \"4\"))) (have I (an J (have K \"5\") (have L \"6\")) (an M (have N \"7\") (have O \"8\")))");
	}
	
}