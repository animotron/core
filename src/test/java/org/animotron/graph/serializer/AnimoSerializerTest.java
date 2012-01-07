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
package org.animotron.graph.serializer;

import org.animotron.ATest;
import org.animotron.expression.JExpression;
import org.animotron.statement.link.LINK;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.animotron.statement.relation.USE;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnimoSerializerTest extends ATest {
	
	@Test
	public void test_00() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(AN._, "X"),
                _(USE._, "Y")
            )
        );
        assertAnimo(A, "the A (X) (use Y).");
    }

    @Test
    public void test_01() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(AN._, "X"),
                _(AN._, "Y")
            )
        );
        assertAnimo(A, "the A (X) (Y).");
    }

    @Test
    public void test_02() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(AN._, "X",
                    _(AN._, "Y")
                )
            )
        );
        assertAnimo(A, "the A X Y.");
    }

    @Test
    public void test_03() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(AN._, "X"),
                _(AN._, "Y")
            )
        );
        assertAnimo(A, "the A (X) (Y).");
    }

    @Test
    public void test_04() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(AN._, "X",
                    _(AN._, "Y")
                )
            )
        );
        assertAnimo(A, "the A X Y.");
    }

    @Test
    public void test_05() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(AN._, "B",
                    _(AN._, "C",
                        _(AN._, "D", value("."))
                    )
                )
            )
        );
        assertAnimo(A, "the A B C D \".\".");
    }

    @Test
    public void test_06() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(AN._, "B",
                    _(AN._, "C",
                        _(AN._, "D"),
                        value(".")
                    )
                )
            )
        );
        assertAnimo(A, "the A B C (D) \".\".");
    }

    @Test
    public void test_07() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(AN._, "B",
                    _(AN._, "C",
                        _(AN._, "D", value(".")),
                        _(AN._, "E", value("_"))
                    )
                )
            )
        );
        assertAnimo(A, "the A B C (D \".\") (E \"_\").");
    }

    @Test
    public void test_08() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(AN._, "X"),
                _(AN._, "B",
                    _(AN._, "C",
                        _(AN._, "D", value(".")),
                        _(AN._, "E", value("_"))
                    )
                )
            )
        );
        assertAnimo(A, "the A (X) (B C (D \".\") (E \"_\")).");
    }

    @Test
    public void test_09() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(AN._, "X"),
                _(AN._, "B",
                    _(AN._, "C",
                        _(AN._, "D", value(".")),
                        _(AN._, "E", value("_"))
                    ),
                    _(AN._, "F",
                        _(AN._, "G", value(":")),
                        _(AN._, "H", value(";"))
                    )
                )
            )
        );
        assertAnimo(A, "the A (X) (B (C (D \".\") (E \"_\")) (F (G \":\") (H \";\"))).");
    }

    @Test
    public void test_0A() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(AN._, "X"),
                _(AN._, "B",
                    _(AN._, "C",
                        _(AN._, "D", value("1")),
                        _(AN._, "E", value("2"))
                    ),
                    _(AN._, "F",
                        _(AN._, "G", value("3")),
                        _(AN._, "H", value("4"))
                    )
                ),
                _(AN._, "I",
                    _(AN._, "J",
                        _(AN._, "K", value("5")),
                        _(AN._, "L", value("6"))
                    ),
                    _(AN._, "M",
                        _(AN._, "N", value("7")),
                        _(AN._, "O", value("8"))
                    )
                )
            )
        );
        assertAnimo(A, "the A (X) (B (C (D \"1\") (E \"2\")) (F (G \"3\") (H \"4\"))) (I (J (K \"5\") (L \"6\")) (M (N \"7\") (O \"8\"))).");
	}

    @Test
    public void test_0B() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                value("bla"), value("bla")
            )
        );
        assertAnimo(A, "the A \"bla\" \"bla\".");
    }
	
    @Test
    public void test_0C() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(AN._, "B"),
                _(AN._, "C")
            )
        );
        assertAnimo(A, "the A (B) (C).");
        assertAnimoResult(A, "the A (B) (C).");
    }

    @Test
    public void test_0D() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                value("B", value("C"))
            )
        );
        assertAnimo(A, "the A \"B\" \"C\".");
    }

    @Test
    public void test_0E() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(_(AN._, "B"), _(AN._, "C"))
            )
        );
        assertAnimo(A, "the A ((B) (C)).");
    }

    @Test
    public void test_0F() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(_(AN._, "B", _(AN._, "C")))
            )
        );
        assertAnimo(A, "the A (B C).");
    }

    @Test
    public void test_10() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(_(AN._, "B"), _(AN._, "C")),
                _(_(AN._, "D"), _(AN._, "E"))
            )
        );
        assertAnimo(A, "the A ((B) (C)) ((D) (E)).");
    }

    @Test
    public void test_11() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(AN._, "B")
            )
        );
        assertAnimo(A, "the A B.");
    }

    @Test
    public void test_12() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(_(AN._, "B"))
            )
        );
        assertAnimo(A, "the A (B).");
    }

    @Test
    public void test_13() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(_(_(AN._, "B")))
            )
        );
        assertAnimo(A, "the A ((B)).");
    }

    @Test
    public void test_14() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(_(_(_(AN._, "B"))))
            )
        );
        assertAnimo(A, "the A (((B))).");
    }

    @Test
    public void test_15() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(LINK._, _(AN._, "B"))
            )
        );
        assertAnimo(A, "the A (B).");
    }

    @Test
    public void test_16() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(LINK._, _(LINK._, _(AN._, "B")))
            )
        );
        assertAnimo(A, "the A ((B)).");
    }

    @Test
    public void test_17() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(LINK._, _(LINK._, _(LINK._, _(AN._, "B"))))
            )
        );
        assertAnimo(A, "the A (((B))).");
    }

    @Test
    public void test_18() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(_(LINK._, _(AN._, "B")))
            )
        );
        assertAnimo(A, "the A ((B)).");
    }

    @Test
    public void test_19() throws Exception {
        JExpression A = new JExpression(
            _(THE._, "A",
                _(LINK._, _(_(AN._, "B")))
            )
        );
        assertAnimo(A, "the A ((B)).");
    }

}
