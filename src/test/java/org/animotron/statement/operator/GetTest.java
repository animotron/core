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
 *  but WITHOUT ALL WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron.statement.operator;

import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.animotron.expression.JExpression;
import org.animotron.statement.query.GET;
import org.animotron.statement.query.SELF;
import org.animotron.statement.relation.IC;
import org.animotron.statement.string.AfterLast;
import org.junit.Test;

import static org.animotron.expression.Expression.__;
import static org.animotron.expression.JExpression.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class GetTest extends ATest {
	
	@Test
	public void getOnManyAN() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "A", _(AN._, "Z", text("A")))
            ),
            new JExpression(
                _(THE._, "B", _(AN._, "Z", text("B")))
            )
        );

    	JExpression d = new JExpression(
			_(THE._, "d", _(GET._, "Z", _(AN._, "A"), _(AN._, "B")))
		);
    	assertAnimoResultOneStep(d, "the d (Z \"A\") (Z \"B\").");

	}

	@Test
	public void getFromPFlow_an_with_param() throws Exception {

    	JExpression.__(new JExpression(
            _(THE._, "A", _(element("B" , _(GET._, "C"))))
        ));
    	JExpression D = new JExpression(
			_(THE._, "D", _(AN._, "A", _(AN._, "C", text("."))))
		);
    	assertAnimoResult(D, "the D A \\B C \".\".");

    	JExpression.__(new JExpression(
            _(THE._, "A1", _(GET._, "B1"))
        ));

    	JExpression d = new JExpression(
			_(THE._, "d", _(AN._, "A1", _(AN._, "B1", text("."))))
		);
    	assertAnimoResult(d, "the d A1, B1 \".\".");

	}
	
	@Test
	public void getFromPFlow_cross_an_with_param() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "A", _(AN._, "B", _(GET._, "C")))
            ),
            new JExpression(
                _(THE._, "D", _(AN._, "E", _(GET._, "B")))
            )
        );
    	
    	JExpression X = new JExpression(
			_(THE._, "X", _(AN._, "D", _(AN._, "A", _(AN._, "C", text(":")))))
		);
    	assertAnimoResultOneStep(X, "the X the D E B C \":\".");

	}

	@Test
	public void getFromPFlow_an_with_an() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "A", _(AN._, "B", _(GET._, "C")))
            ),
            new JExpression(
                _(THE._, "D", _(AN._, "C", text(".")))
            )
        );

    	JExpression E = new JExpression(
			_(THE._, "E", _(AN._, "A", _(AN._, "D")))
		);
    	assertAnimoResultOneStep(E, "the E the A B C \".\".");

	}
	
	@Test
	public void getFromPFlow_an_with_more_an() throws Exception {

    	JExpression.__(
            new JExpression(
                _(THE._, "A", _(AN._, "B", _(GET._, "C")))
            ),
            new JExpression(
                _(THE._, "D", _(AN._, "C", text(".")))
            ),
            new JExpression(
                _(THE._, "E", _(AN._, "C", text(":")))
            )
        );

    	JExpression F = new JExpression(
			_(THE._, "F", _(AN._, "A", _(AN._, "D"), _(AN._, "E", _(AN._, "C", text("_")))))
		);
    	assertAnimoResultOneStep(F, "the F the A B (C \".\") (C \"_\").");

	}
	
    @Test
    public void checkIConIS() throws Exception {

        JExpression.__(

            new JExpression(
                _(THE._, "A",
                    _(AN._, "A1", text("some.path")),

                    _(IC._, "A2",
                        _(SELF._, "A1")),

                    _(IC._, "B1",
                        _(AfterLast._,
                            text("."),
                            _(SELF._, "A1")))
                )),

            new JExpression(
                _(THE._, "B",
                    _(AN._, "A"),
                    _(AN._, "A1", text("test.txt"))
                ))

        );

        JExpression C0 = new JExpression(
        _(THE._, "C0",
            _(GET._, "A1",
                _(AN._, "A")
        )));
        assertAnimoResultOneStep(C0, "the C0 A1 \"some.path\".");

        JExpression C1 = new JExpression(
        _(THE._, "C1",
            _(GET._, "A1",
                _(AN._, "B")
        )));
        assertAnimoResultOneStep(C1, "the C1 A1 \"test.txt\".");

        JExpression C2 = new JExpression(
        _(THE._, "C2",
            _(GET._, "A2",
                _(AN._, "B")
        )));
        //XXX: assertAnimoResult(C2, "the C2 A2 A1 \"test.txt\".");
        assertAnimoResultOneStep(C2, "the C2 ic A2 A1 \"test.txt\".");

        JExpression C3 = new JExpression(
        _(THE._, "C3",
            _(GET._, "E1",
                _(AN._, "B")
        )));
        assertAnimoResultOneStep(C3, "the C3.");

        JExpression C4 = new JExpression(
        _(THE._, "C4",
            _(GET._, "B1",
                _(AN._, "B")
        )));
        //XXX: assertAnimoResult(C4, "the C4 B1 \"txt\".");
        assertAnimoResultOneStep(C4, "the C4 ic B1 \"txt\".");

    }

    @Test
    public void checkHAVEonIS() throws Exception {

        JExpression.__(

            new JExpression(
                _(THE._, "A",
                    _(AN._, "A1", text("some.path")),

                    _(AN._, "A2",
                        _(SELF._, "A1")),

                    _(AN._, "B1",
                        _(AfterLast._,
                            text("."),
                            _(SELF._, "A1")))
                )),

            new JExpression(
                _(THE._, "B",
                    _(AN._, "A"),
                    _(AN._, "A1", text("test.txt"))
                ))

        );

        JExpression C0 = new JExpression(
        _(THE._, "C0",
            _(GET._, "A1",
                _(AN._, "A")
        )));
        assertAnimoResultOneStep(C0, "the C0 A1 \"some.path\".");

        JExpression C1 = new JExpression(
        _(THE._, "C1",
            _(GET._, "A1",
                _(AN._, "B")
        )));
        assertAnimoResultOneStep(C1, "the C1 A1 \"test.txt\".");

        JExpression C2 = new JExpression(
        _(THE._, "C2",
            _(GET._, "A2",
                _(AN._, "B")
        )));
        assertAnimoResultOneStep(C2, "the C2 A2 A1 \"test.txt\".");

        JExpression C3 = new JExpression(
        _(THE._, "C3",
            _(GET._, "E1",
                _(AN._, "B")
        )));
        assertAnimoResultOneStep(C3, "the C3.");

        JExpression C4 = new JExpression(
        _(THE._, "C4",
            _(GET._, "B1",
                _(AN._, "B")
        )));
        assertAnimoResultOneStep(C4, "the C4 B1 \"txt\".");

    }

    @Test
    public void getFromPFlow_an_with_stack() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "A", _(GET._, "X"))
            ),
            new JExpression(
                _(THE._, "B", _(GET._, "Y"), _(AN._, "A"))
            ),
            new JExpression(
                _(THE._, "C", _(GET._, "Z"), _(AN._, "B"))
            )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(AN._, "C", _(AN._, "X", text("α")), _(AN._, "Y", text("β")), _(AN._, "Z", text("γ"))))
        );
        assertAnimoResultOneStep(E, "the E the C (Z \"γ\") (the B (Y \"β\") (the A X \"α\")).");

    }

    @Test
    public void getFromPFlow_more_an_with_stack() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "A", _(GET._, "X"), _(AN._, "Z", text("γ")))
            ),
            new JExpression(
                _(THE._, "B", _(GET._, "Y"), _(AN._, "A"))
            ),
            new JExpression(
                _(THE._, "C", _(GET._, "Z"))
            )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(AN._, "C", _(AN._, "B", _(AN._, "B", text("β")))))
        );
        assertAnimoResultOneStep(E, "the E the C Z \"γ\".");

    }

    @Test
    public void getFromPFlow_one_more_an_with_stack() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "A", _(GET._, "X"), _(AN._, "Z", text("γ")))
            ),
            new JExpression(
                _(THE._, "B", _(GET._, "Y"))
            ),
            new JExpression(
                _(THE._, "C", _(GET._, "Z"))
            )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(AN._, "C", _(AN._, "B", _(AN._, "A"))))
        );
        assertAnimoResultOneStep(E, "the E the C Z \"γ\".");
    }

    @Test
    public void getFromPFlow_an_an_an() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "A", _(AN._, "Y", _(GET._, "X")))
            ),
            new JExpression(
                _(THE._, "B", _(AN._, "Z", _(GET._, "Y")))
            ),
            new JExpression(
                _(THE._, "C", _(GET._, "Z"))
            )
        );

        JExpression D = new JExpression(
            _(THE._, "D", _(AN._, "C", _(AN._, "B", _(AN._, "A", _(AN._, "X", text("."))))))
        );
        assertAnimoResultOneStep(D, "the D the C Z Y X \".\".");

    }
    
    @Test
	public void test_01() throws Exception {
        __(
            new AnimoExpression("the a (x 1) (y 2) (z 3)"),
            new AnimoExpression("the b (get x) (get y) (get z)")
        );
        Expression e = new AnimoExpression("b a");
        assertAnimoResultOneStep(e, "123");
	}

    @Test
	public void test_02() throws Exception {
        __(
                new AnimoExpression("the a (x 1)"),
                new AnimoExpression("the b (x 2)"),
                new AnimoExpression("the c (x 3)")
        );
        Expression e = new AnimoExpression("get x (a) (b) (c)");
        assertAnimoResultOneStep(e, "have x 1. x 2. x 3.");
	}

    @Test
	public void test_03() throws Exception {
        __(
            new AnimoExpression("the a x 1"),
            new AnimoExpression("the b x 2"),
            new AnimoExpression("the c x 3")
        );
        Expression e = new AnimoExpression("get x a,b,c");
        assertAnimoResultOneStep(e, "have x 1. x 2. x 3.");
	}
    
	@Test
	public void test_04() throws Exception {
		testAnimo("the user1 (user) (name \"user1\").");

		testAnimo("the item1 (item) (name \"item1\").");
		
		assertAnimoResultOneStep(
            new AnimoExpression("get name (user1) (item1)."),
            "have name \"user1\". name \"item1\"."
        );

		assertAnimoResultOneStep(
            new AnimoExpression("get name (user1, item1)."),
            "have name \"user1\". name \"item1\"."
        );

		assertAnimoResultOneStep(
            new AnimoExpression("get name user1,item1."),
            "have name \"user1\". name \"item1\"."
        );
	}


}