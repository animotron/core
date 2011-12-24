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
import org.animotron.statement.relation.SHALL;
import org.animotron.statement.string.AfterLast;
import org.junit.Ignore;
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
                _(THE._, "A", _(AN._, "Z", value("A")))
            ),
            new JExpression(
                _(THE._, "B", _(AN._, "Z", value("B")))
            )
        );

    	JExpression test = new JExpression(
			_(THE._, "d", _(GET._, "Z", _(AN._, "A"), _(AN._, "B")))
		);
    	assertAnimoResultOneStep(test, "the d (Z \"A\") (Z \"B\").");

    	test = new JExpression(
			_(GET._, "Z", _(AN._, "A"), _(AN._, "B"))
		);
    	assertAnimoResultOneStep(test, "Z \"A\". Z \"B\".");
	}

	@Test
	public void getFromPFlow_an_with_param() throws Exception {

    	JExpression.__(new JExpression(
            _(THE._, "A", element("B" , _(GET._, "C")))
        ));
    	JExpression test = new JExpression(
			_(AN._, "A", _(AN._, "C", value(".")))
		);
    	assertAnimoResult(test, "A \\B C \".\".");

    	JExpression.__(new JExpression(
            _(THE._, "A1", _(GET._, "B1"))
        ));

    	test = new JExpression(
			_(AN._, "A1", _(AN._, "B1", value(".")))
		);
    	assertAnimoResult(test, "A1 B1 \".\".");
	}
	
	@Test
	public void getFromPFlow_cross_an_with_param() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "A", _(AN._, "B", _(GET._, "C")))
            ),
            new JExpression(
                _(THE._, "D", element("E", _(GET._, "B")))
            )
        );
    	
    	JExpression test = new JExpression(
			_(AN._, "D", _(AN._, "A", _(AN._, "C", value(":"))))
		);
    	assertAnimoResult(test, "D \\E B C \":\".");
	}

	@Test
	public void getFromPFlow_an_with_an() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "A", element("B", _(GET._, "C")))
            ),
            new JExpression(
                _(THE._, "D", _(AN._, "C", value(".")))
            )
        );

    	JExpression test = new JExpression(
			_(AN._, "A", _(AN._, "D"))
		);
    	assertAnimoResult(test, "A \\B C \".\".");
	}
	
	@Test
	public void getFromPFlow_an_with_more_an() throws Exception {

    	JExpression.__(
            new JExpression(
                _(THE._, "A", element("B", _(GET._, "C")))
            ),
            new JExpression(
                _(THE._, "D", _(AN._, "C", value(".")))
            ),
            new JExpression(
                _(THE._, "E", _(AN._, "C", value(":")))
            )
        );

    	JExpression test = new JExpression(
			_(THE._, "F", _(AN._, "A", _(AN._, "D"), _(AN._, "E", _(AN._, "C", value("_")))))
		);
    	assertAnimoResult(test, "the F A \\B (C \".\") (C \"_\").");

	}
	
    @Test
    @Ignore
    public void checkSHALLonISandSELF() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "A",
                    _(AN._, "A1", value("some.path")),

                    _(SHALL._, "A2",
                        _(GET._, "A1")),

                    _(SHALL._, "B1",
                        _(AfterLast._,
                            value("."),
                            _(GET._, "A1")))
                )),

            new JExpression(
                _(THE._, "B",
                    _(AN._, "A"),
                    _(AN._, "A1", value("test.txt"))
                ))

        );

        JExpression C0 = new JExpression(
        _(THE._, "C0",
            _(GET._, "A1",
                _(AN._, "A")
        )));
        assertAnimoResult(C0, "the C0 A1 \"some.path\".");

        JExpression C1 = new JExpression(
        _(THE._, "C1",
            _(GET._, "A1",
                _(AN._, "B")
        )));
        assertAnimoResult(C1, "the C1 A1 \"test.txt\".");

        JExpression C2 = new JExpression(
        _(THE._, "C2",
            _(GET._, "A2",
                _(AN._, "B")
        )));
        //XXX: assertAnimoResult(C2, "the C2 A2 A1 \"test.txt\".");
        assertAnimoResult(C2, "the C2 shall A2 A1 \"test.txt\".");

        JExpression C3 = new JExpression(
        _(THE._, "C3",
            _(GET._, "E1",
                _(AN._, "B")
        )));
        assertAnimoResult(C3, "the C3.");

        JExpression C4 = new JExpression(
        _(THE._, "C4",
            _(GET._, "B1",
                _(AN._, "B")
        )));
        //XXX: assertAnimoResult(C4, "the C4 B1 \"txt\".");
        assertAnimoResult(C4, "the C4 shall B1 \"txt\".");
    }

    @Test
    @Ignore
    public void checkHAVEonISandSELF() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "A",
                    _(AN._, "A1", value("some.path")),

                    _(SHALL._, "A2",
                        _(GET._, "A1")),

                    _(SHALL._, "B1",
                        _(AfterLast._,
                            value("."),
                            _(GET._, "A1")))
                )),

            new JExpression(
                _(THE._, "B",
                    _(AN._, "A"),
                    _(AN._, "A1", value("test.txt"))
                ))

        );

        JExpression C0 = new JExpression(
        _(THE._, "C0",
            _(GET._, "A1",
                _(AN._, "A")
        )));
        assertAnimoResult(C0, "the C0 A1 \"some.path\".");

        JExpression C1 = new JExpression(
        _(THE._, "C1",
            _(GET._, "A1",
                _(AN._, "B")
        )));
        assertAnimoResult(C1, "the C1 A1 \"test.txt\".");

        JExpression C2 = new JExpression(
        _(THE._, "C2",
            _(GET._, "A2",
                _(AN._, "B")
        )));
        assertAnimoResult(C2, "the C2 A2 A1 \"test.txt\".");

        JExpression C3 = new JExpression(
        _(THE._, "C3",
            _(GET._, "E1",
                _(AN._, "B")
        )));
        assertAnimoResult(C3, "the C3.");

        JExpression C4 = new JExpression(
        _(THE._, "C4",
            _(GET._, "B1",
                _(AN._, "B")
        )));
        assertAnimoResult(C4, "the C4 B1 \"txt\".");
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
            _(THE._, "E", _(AN._, "C", _(AN._, "X", value("α")), _(AN._, "Y", value("β")), _(AN._, "Z", value("γ"))))
        );
        assertAnimoResult(E, "the E C (Z \"γ\") (B (Y \"β\") (A X \"α\")).");
    }

    @Test
    public void getFromPFlow_more_an_with_stack() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "A", _(GET._, "X"), _(AN._, "Z", value("γ")))
            ),
            new JExpression(
                _(THE._, "B", _(GET._, "Y"), _(AN._, "A"))
            ),
            new JExpression(
                _(THE._, "C", _(GET._, "Z"))
            )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(AN._, "C", _(AN._, "B", _(AN._, "B", value("β")))))
        );
        assertAnimoResult(E, "the E C Z \"γ\".");

    }

    @Test
    public void getFromPFlow_one_more_an_with_stack() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "A", _(GET._, "X"), _(AN._, "Z", value("γ")))
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
        assertAnimoResult(E, "the E C Z \"γ\".");
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
            _(THE._, "D", _(AN._, "C", _(AN._, "B", _(AN._, "A", _(AN._, "X", value("."))))))
        );
        assertAnimoResult(D, "the D C Z Y X \".\".");
    }
    
    @Test
	public void test_01() throws Exception {
        __(
            new AnimoExpression("the a (x 1) (y 2) (z 3)"),
            new AnimoExpression("the b (get x) (get y) (get z)")
        );
        Expression e = new AnimoExpression("b a");
        assertStringResult(e, "123");
	}

    @Test
	public void test_02() throws Exception {
        __(
                new AnimoExpression("the a (x 1)"),
                new AnimoExpression("the b (x 2)"),
                new AnimoExpression("the c (x 3)")
        );
        Expression e = new AnimoExpression("get x (a) (b) (c)");
        assertAnimoResult(e, "x 1. x 2. x 3.");
	}

    @Test
	public void test_03() throws Exception {
        __(
            new AnimoExpression("the a x 1"),
            new AnimoExpression("the b x 2"),
            new AnimoExpression("the c x 3")
        );
        Expression e = new AnimoExpression("get x a,b,c");
        assertAnimoResult(e, "x 1. x 2. x 3.");
	}
    
	@Test
	public void test_04() throws Exception {
		testAnimo("the user1 (user) (name \"user1\").");

		testAnimo("the item1 (item) (name \"item1\").");
		
		assertAnimoResult(
            new AnimoExpression("get name (user1) (item1)."),
            "name \"user1\". name \"item1\"."
        );

		assertAnimoResult(
            new AnimoExpression("get name (user1, item1)."),
            "name \"user1\". name \"item1\"."
        );

		assertAnimoResult(
            new AnimoExpression("get name user1,item1."),
            "name \"user1\". name \"item1\"."
        );
	}

	@Test
	public void test_05() throws Exception {
		testAnimo("the site host \"localhost\".");
		
		testAnimo("the resource get host.");
		
		assertAnimoResult("an site, resource", "site host. resource host \"localhost\".");
		
		assertAnimoResult("an resource, site", "resource host \"localhost\". site host.");
	}

	@Test
	public void test_06() throws Exception {
		testAnimo("the file (extension \"txt\") (any mime-type with extension get extension).");
		
		testAnimo("the text-plain (mime-type) (text) (type \"text/plain\") (name \"Plain text\") (extension \"txt\" \"text\").");
		testAnimo("the text-html (mime-type) (text) (type \"text/html\") (name \"HTML Document\") (extension \"htm\" \"html\").");
		
		assertAnimoResult("any mime-type with extension \"txt\"", "text-plain (mime-type) (text) (type \"text/plain\") (name \"Plain text\") (extension (\"txt\") (\"text\")).");

		assertAnimoResult("get mime-type file", "text-plain (mime-type) (text) (type \"text/plain\") (name \"Plain text\") (extension (\"txt\") (\"text\")).");
	}

    @Test
    public void test_07() throws Exception {
        testAnimo("the male sex.");
        testAnimo("the john male.");

        assertAnimoResult("get sex john", "male.");
        assertAnimoResult("any male", "the john male sex.");
        assertAnimoResult("get sex any male", "male.");
    }

    @Test
    public void test_08() throws Exception {
        testAnimo("the male sex.");
        testAnimo("the man male.");
        testAnimo("the john man.");

        assertAnimoResult("get sex john", "man.");
    }

    @Test
    @Ignore
    public void test_09() throws Exception {
        testAnimo("the male sex.");
        testAnimo("the john male.");

        testAnimo("the _man_ john.");
        assertAnimoResult("get sex _man_", "male.");

        testAnimo("the man any male.");

        assertAnimoResult("get sex man", "male.");
    }

    @Test
    public void test_10() throws Exception {
        testAnimo("the male sex.");
        testAnimo("the john person, male.");

        assertAnimoResult("any person", "the john (person) (male sex).");
        assertAnimoResult("get sex any person", "person, male.");
    }

    @Test
    public void test_11() throws Exception {
        testAnimo("the male sex.");
        testAnimo("the john person, male.");
        testAnimo("the man any person.");

        assertAnimoResult("get sex man", "person, male.");
    }

    @Test
    public void test_12() throws Exception {
        testAnimo("the x b, c 1.");
        assertAnimoResult("get c, b x", "b, c 1.");
        assertAnimoResult("get c x", "b, c 1.");
        assertAnimoResult("get b x", "b, c 1.");
    }

}
