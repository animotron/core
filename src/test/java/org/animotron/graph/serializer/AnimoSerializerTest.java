/*
 *  http://animotron.org
 *
 *  This program is free software; you credistribute it and/or
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
	public void test_00() throws Exception {
        Expression A = new Expression(
            _(THE._, "A",
                _(IS._, "X"),
                _(USE._, "Y")
            )
        );
        assertAnimo(A, "the A (is X) (use Y)");
    }

    @Test
    public void test_01() throws Exception {
        Expression A = new Expression(
            _(THE._, "A",
                _(HAVE._, "X"),
                _(HAVE._, "Y")
            )
        );
        assertAnimo(A, "the A (have X) (have Y)");
    }

    @Test
    public void test_02() throws Exception {
        Expression A = new Expression(
            _(THE._, "A",
                _(HAVE._, "X",
                    _(HAVE._, "Y")
                )
            )
        );
        assertAnimo(A, "the A have X have Y");
    }

    @Test
    public void test_03() throws Exception {
        Expression A = new Expression(
            _(THE._, "A",
                _(AN._, "X"),
                _(AN._, "Y")
            )
        );
        assertAnimo(A, "the A (X) (Y)");
    }

    @Test
    public void test_04() throws Exception {
        Expression A = new Expression(
            _(THE._, "A",
                _(AN._, "X",
                    _(AN._, "Y")
                )
            )
        );
        assertAnimo(A, "the A X Y");
    }

    @Test
    public void test_05() throws Exception {
        Expression A = new Expression(
            _(THE._, "A",
                _(HAVE._, "B",
                    _(AN._, "C",
                        _(HAVE._, "D", text("."))
                    )
                )
            )
        );
        assertAnimo(A, "the A have B C have D \".\"");
    }

    @Test
    public void test_06() throws Exception {
        Expression A = new Expression(
            _(THE._, "A",
                _(HAVE._, "B",
                    _(AN._, "C",
                        _(HAVE._, "D"),
                        text(".")
                    )
                )
            )
        );
        assertAnimo(A, "the A have B C (have D) (\".\")");
    }

    @Test
    public void test_07() throws Exception {
        Expression A = new Expression(
            _(THE._, "A",
                _(HAVE._, "B",
                    _(AN._, "C",
                        _(HAVE._, "D", text(".")),
                        _(HAVE._, "E", text("_"))
                    )
                )
            )
        );
        assertAnimo(A, "the A have B C (have D \".\") (have E \"_\")");
    }

    @Test
    public void test_08() throws Exception {
        Expression A = new Expression(
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
        assertAnimo(A, "the A (is X) (have B C (have D \".\") (have E \"_\"))");
    }

    @Test
    public void test_09() throws Exception {
        Expression A = new Expression(
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
        assertAnimo(A, "the A (is X) (have B (C (have D \".\") (have E \"_\")) (F (have G \":\") (have H \";\")))");
    }

    @Test
    public void test_0A() throws Exception {
        Expression A = new Expression(
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
                    _(AN._, "M",
                        _(HAVE._, "N", text("7")),
                        _(HAVE._, "O", text("8"))
                    )
                )
            )
        );
        assertAnimo(A, "the A (is X) (have B (C (have D \"1\") (have E \"2\")) (F (have G \"3\") (have H \"4\"))) (have I (J (have K \"5\") (have L \"6\")) (M (have N \"7\") (have O \"8\")))");
	}

    @Test
    public void test_0B() throws Exception {
        Expression A = new Expression(
            _(THE._, "A",
                text("bla"), text("bla")
            )
        );
        assertAnimo(A, "the A (\"bla\") (\"bla\")");
    }
	
    @Test
    public void test_0C() throws Exception {
        Expression A = new Expression(
            _(THE._, "A",
                _(AN._, "B"),
                _(AN._, "C")
            )
        );
        assertAnimoResult(A, "the A (the B) (the C)");
    }

    @Test
    public void test_0D() throws Exception {
        Expression A = new Expression(
            _(THE._, "A",
                text("B", text("C"))
            )
        );
        assertAnimo(A, "the A \"B\" \"C\"");
    }

    @Test
    public void test_0E() throws Exception {
        Expression A = new Expression(
            _(THE._, "A",
                _(_(AN._, "B"), _(AN._, "C"))
            )
        );
        assertAnimo(A, "the A ((B) (C))");
    }

    @Test
    public void test_0F() throws Exception {
        Expression A = new Expression(
            _(THE._, "A",
                _(_(AN._, "B", _(AN._, "C")))
            )
        );
        assertAnimo(A, "the A (B C)");
    }

    @Test
    public void test_10() throws Exception {
        Expression A = new Expression(
            _(THE._, "A",
                _(_(AN._, "B"), _(AN._, "C")),
                _(_(AN._, "D"), _(AN._, "E"))
            )
        );
        assertAnimo(A, "the A ((B) (C)) ((D) (E))");
    }

}
