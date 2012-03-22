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
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.animotron.expression.JExpression;
import org.animotron.statement.query.GET;
import org.animotron.statement.relation.SHALL;
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
	public void getOnManyAN() throws Throwable {

        __(
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
    	assertAnimoResultOneStep(test, "the d", " (Z \"A\")", " (Z \"B\")", ".");

    	test = new JExpression(
			_(GET._, "Z", _(AN._, "A"), _(AN._, "B"))
		);
    	assertAnimoResultOneStep(test, "Z \"A\".", "Z \"B\".");
	}

	@Test
	public void getFromPFlow_an_with_param() throws Throwable {

    	__(new JExpression(
            _(THE._, "A", element("B" , _(GET._, "C")))
        ));
    	JExpression test = new JExpression(
			_(AN._, "A", _(AN._, "C", value(".")))
		);
    	assertAnimoResult(test, "A \\B C \".\".");

    	__(new JExpression(
            _(THE._, "A1", _(GET._, "B1"))
        ));

    	test = new JExpression(
			_(AN._, "A1", _(AN._, "B1", value(".")))
		);
    	assertAnimoResult(test, "A1 B1 \".\".");
	}
	
	@Test
	public void getFromPFlow_cross_an_with_param() throws Throwable {

        __(
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
	public void getFromPFlow_an_with_an() throws Throwable {

        __(
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
	public void getFromPFlow_an_with_more_an() throws Throwable {

    	__(
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
    public void getFromPFlow_an_with_stack() throws Throwable {

        __(
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
            _(AN._, "C", _(AN._, "X", value("α")), _(AN._, "Y", value("β")), _(AN._, "Z", value("γ")))
        );
        assertAnimoResult(E, "C (Z \"γ\") (B (Y \"β\") (A X \"α\")).");
    }

    @Test
    public void getFromPFlow_more_an_with_stack() throws Throwable {

        __(
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
    public void getFromPFlow_one_more_an_with_stack() throws Throwable {

        __(
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
    public void getFromPFlow_an_an_an() throws Throwable {

        __(
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
	public void test_01() throws Throwable {
        __(
            new AnimoExpression("the a (x 1) (y 2) (z 3)"),
            new AnimoExpression("the b (get x) (get y) (get z)")
        );
        Expression e = new AnimoExpression("b a");
        assertStringResult(e, "123");
	}

    @Test
	public void test_02() throws Throwable {
        __(
                new AnimoExpression("the a (x 1)"),
                new AnimoExpression("the b (x 2)"),
                new AnimoExpression("the c (x 3)")
        );
        Expression e = new AnimoExpression("get x (a) (b) (c)");
        assertAnimoResult(e, "x 1.", "x 2.", "x 3.");
	}

    @Test
	public void test_03() throws Throwable {
        __(
            new AnimoExpression("the a x 1"),
            new AnimoExpression("the b x 2"),
            new AnimoExpression("the c x 3")
        );
        Expression e = new AnimoExpression("get x a,b,c");
        assertAnimoResult(e, "x 1. x 2. x 3.");
	}
    
	@Test
	public void test_04() throws Throwable {
		testAnimo("the user1 (user) (name \"user1\").");

		testAnimo("the item1 (item) (name \"item1\").");
		
		assertAnimoResult(
            new AnimoExpression("get name (user1) (item1)."),
            "name \"user1\".", "name \"item1\"."
        );

		assertAnimoResult(
            new AnimoExpression("get name (user1, item1)."),
            "name \"user1\".", "name \"item1\"."
        );

		assertAnimoResult(
            new AnimoExpression("get name user1,item1."),
            "name \"user1\".", "name \"item1\"."
        );
	}

	@Test
	@Ignore
	public void test_05() throws Throwable {
		testAnimo("the site host \"localhost\".");
		
		testAnimo("the resource get host.");
		
		assertAnimoResult("an site, resource", "site host. resource host \"localhost\".");
		
		assertAnimoResult("an resource, site", "resource host \"localhost\". site host.");
	}

	@Test
	public void test_06() throws Throwable {
		testAnimo("the file (extension \"txt\") (any mime-type with extension get extension).");
		
		testAnimo("the text-plain (mime-type) (text) (type \"text/plain\") (name \"Plain text\") (extension \"txt\" \"text\").");
		testAnimo("the text-html (mime-type) (text) (type \"text/html\") (name \"HTML Document\") (extension \"htm\" \"html\").");
		
		//assertAnimoResultOneStep("any mime-type with extension \"txt\"", "the text-plain (mime-type) (text) (type \"text/plain\") (name \"Plain text\") (extension \"txt\" \"text\").");

		assertAnimoResultOneStep("get mime-type file", "the text-plain (mime-type) (text) (type \"text/plain\") (name \"Plain text\") (extension \"txt\" \"text\").");
	}

    @Test
    public void test_07() throws Throwable {
        testAnimo("the male sex.");
        testAnimo("the john male.");

        assertAnimoResult("get sex john", "male.");
        assertAnimoResult("any male", "the john male sex.");
        assertAnimoResult("get sex any male", "male.");
    }

    @Test
    public void test_08() throws Throwable {
        testAnimo("the male sex.");
        testAnimo("the man male.");
        testAnimo("the john man.");

        assertAnimoResult("get sex john", "male.");
    }

    @Test
    public void test_09() throws Throwable {
        testAnimo("the male sex.");
        testAnimo("the john male.");

        testAnimo("the _man_ john.");
        assertAnimoResult("get sex _man_", "male.");

        testAnimo("the man any male.");

        assertAnimoResult("get sex man", "male.");
    }

    @Test
    public void test_10() throws Throwable {
        testAnimo("the male sex.");
        testAnimo("the john person, male.");

        assertAnimoResult("any person", "the john (person) (male sex).");
        assertAnimoResult("get sex any person", "person, male.");
    }

    @Test
    @Ignore
    public void test_11() throws Throwable {
        testAnimo("the male sex.");
        testAnimo("the john person, male.");
        testAnimo("the man any person.");

        assertAnimoResult("get sex man", "person, male.");
    }

    @Test
    public void test_12() throws Throwable {
        testAnimo("the x b, c 1.");
        assertAnimoResult("get c, b x", "b, c 1.");
        assertAnimoResult("get c x", "b, c 1.");
        assertAnimoResult("get b x", "b, c 1.");
    }

    @Test
    public void test_13() throws Throwable {
        testAnimo("the a b c.");
        testAnimo("the x get b.");
        assertAnimoResult("x a", "x b c.");
    }

    @Test
    public void test_14() throws Throwable {
        testAnimo("the a b c.");
        testAnimo("the x get b.");
        assertAnimoResult("x b get b a", "x b b c.");//UNDERSTAND: is it correct?
    }

    @Test
    public void test_15() throws Throwable {
        testAnimo("the a b c.");
        testAnimo("the x get b.");
        assertAnimoResult("x get b a", "x b c.");
    }

    @Test
    public void test_16() throws Throwable {
        testAnimo("the x (foo) (bar).");
        testAnimo("the y x.");
        assertAnimoResult("get foo y", "x.");
    }

    @Test
    public void test_17() throws Throwable {
        testAnimo("the x bar.");
        assertAnimoResult("get bar x", "bar.");
    }

    @Test
    public void test_18() throws Throwable {
        testAnimo("the x foo z bar y.");
        assertAnimoResult("get bar x", "bar y.");
    }

    @Test
    public void test_19() throws Throwable {
        testAnimo("the x a 1.");
        testAnimo("the y a 2 (x).");
        assertAnimoResult("get a y", "a 2 (x a).");
    }

    @Test
    public void test_20() throws Throwable {
        testAnimo("the x a 1.");
        testAnimo("the y a 2 (x).");
        assertAnimoResult("each (get a y) (this a)", "2. x.");
    }

    @Test
    public void test_21() throws Throwable {
        testAnimo("the x a 1.");
        testAnimo("the y a 2 (b x).");
        assertAnimoResult("get a y", "a 2 (b).");
    }

    @Test
    public void test_22() throws Throwable {
        testAnimo("the x a 1.");
        testAnimo("the y a 2 (b x).");
        assertAnimoResult("each (get a y) (this a)", "2. b.");
    }

    @Test
    public void test_23() throws Throwable {
        testAnimo("the x a z.");
        testAnimo("the y a 2 (b c x).");
        assertAnimoResult("get a y", "a 2 (b).");
    }

    @Test
    public void test_24() throws Throwable {
        testAnimo("the x a z.");
        testAnimo("the y a 2 (b c x).");
        assertAnimoResult("each (get a y) (this a)", "2. b.");
    }

    @Test
    public void test_25() throws Throwable {
        testAnimo("the x \\a y z.");
        assertAnimoResult("get y x", "");
    }

    @Test
    public void test_26() throws Throwable {
        testAnimo("the x y z.");
        testAnimo("the z \\bar.");
        assertAnimoResult("the foo an (get y x)", "the foo y z \\bar.");
    }

    @Test
    public void test_27() throws Throwable {
        testAnimo("the x y z.");
        testAnimo("the z \\bar.");
        assertAnimoResult("the foo (x) (an (get y))", "the foo (x y) (y z \\bar).");
    }

    @Test
    public void test_28() throws Throwable {
        testAnimo("the x y z.");
        testAnimo("the z get bar.");
        testAnimo("the a bar 0.");
        assertAnimoResult("the foo (a) (x) (an (get y))", "the foo (a bar) (x y) (y z bar 0).");
    }

    @Test
    public void test_29() throws Throwable {
        testAnimo("the foo bar.");
        testAnimo("the x y foo.");
        assertAnimoResult("get foo x", "foo.");
        assertAnimoResult("get bar x", "foo.");
        assertAnimoResult("get foo get y x", "foo.");
        assertAnimoResult("get bar get y x", "foo.");
    }

    @Test
    public void test_29_() throws Throwable {
        testAnimo("the bar 1.");
        testAnimo("the x y foo bar.");
        assertAnimoResult("get foo x", "foo bar 1."); 
    }

    @Test
    public void test_29__() throws Throwable {
        testAnimo("the bar 1.");
        testAnimo("the x y foo bar.");
        assertAnimoResult("get foo get y x", "foo bar 1.");
    }

    @Test
    //XXX: this looks very wrong...
    public void test_30() throws Throwable {
        testAnimo("the x y foo.");
        assertAnimoResult("get foo x", "foo.");
        assertAnimoResult("get foo get y x", "foo.");
        assertAnimoResult("the q1 (x) (get foo)", "the q1 (x y) (y foo).");
        assertAnimoResult("the q2 (x) (get foo get y)", "the q2 (x y) (y sfoo).");
        assertAnimoResult("the q3 (x) (an get foo)", "the q3 (x y) (foo).");
        assertAnimoResult("the q4 (x) (an get foo get y)", "the q4 (x y) (foo).");
    }

    @Test
    //XXX: this looks veeeeery wrong...
    public void test_30_() throws Throwable {
        testAnimo("the x y foo.");
        assertAnimoResult("get foo x", "foo.");
        assertAnimoResult("get foo get y x", "foo.");
        assertAnimoResult("the q1 (x) (\\p get foo)", "the q1 (x y) (\\p foo).");
        assertAnimoResult("the q2 (x) (\\p get foo get y)", "the q2 (x y) (\\p foo).");
        assertAnimoResult("the q3 (x) (\\p an get foo)", "the q3 (x y) (\\p foo).");
        assertAnimoResult("the q4 (x) (\\p an get foo get y)", "the q4 (x y) (\\p foo).");
    }

    @Test
    public void test_31() throws Throwable {
        testAnimo("the bar z.");
        testAnimo("the foo1 (bar) 1.");
        testAnimo("the foo2 (bar) 2.");
        testAnimo("the x y foo1.");
        assertAnimoResult("get bar x", "foo1.");
    }

    @Test
    public void test_32() throws Throwable {
        testAnimo("the bar z.");
        testAnimo("the foo1 (bar) 1.");
        testAnimo("the foo2 (bar) 2.");
        testAnimo("the x y (foo3) (foo1).");
        assertAnimoResult("get bar x", "foo1.");
    }

    @Test
    public void test_33() throws Throwable {
        testAnimo("the bar z.");
        testAnimo("the foo1 (bar) 1.");
        testAnimo("the foo2 (bar) 2.");
        assertAnimoResult("get bar get y x", "");//"foo1."
        //XXX: answer '' correct because get y x return ''
    }

    @Test
    public void test_34() throws Throwable {
        testAnimo("the bar z.");
        testAnimo("the foo1 (bar) (get a).");
        testAnimo("the foo2 (bar) 2.");
        assertAnimoResult("foo1 a 1", "foo1 (bar z) (a 1).");
        assertAnimoResult("an (an foo1) (a 1)", "foo1 (bar z) (a 1).");
        //assertAnimoResult("an (get bar get y x) (a 1)", "foo1 a 1."); //answer '' correct because (get bar get y x) == ''
    }

    @Test
    public void test_34_() throws Throwable {
        testAnimo("the bar z.");
        testAnimo("the foo1 (bar) (\\p get a).");
        testAnimo("the foo2 (bar) 2.");
        assertAnimoResult("foo1 a 1", "foo1 (bar z) (\\p a 1).");
        assertAnimoResult("an (an foo1) (a 1)", "foo1 (bar z) (\\p a 1).");
        //assertAnimoResult("an (get bar get y x) (a 1)", "foo1 \\p a 1."); //answer '' correct because (get bar get y x) == ''
    }

    @Test
    public void test_35() throws Throwable {
        testAnimo("the bar z.");
        testAnimo("the foo1 (bar) 1.");
        testAnimo("the foo2 (bar) 2.");
        testAnimo("the x y foo3, foo1.");
        assertAnimoResult("get bar x", "y (foo3) (foo1 (bar z) 1).");
    }

    @Test
    public void test_36() throws Throwable {
        testAnimo("the bar z.");
        testAnimo("the foo1 (bar) 1.");
        testAnimo("the foo2 (bar) 2.");
        testAnimo("the x y foo3, foo1.");
        assertAnimoResult("get bar get y x", "foo3, foo1.");
    }

    @Test
    public void test_37() throws Throwable {
        testAnimo("the x a b.");
        testAnimo("the y (x) (a c).");
        assertAnimoResult("get a y", "a c.");
        //assertAnimoResult("get b get a y", "a b.");
    }

    @Test
    public void test_38() throws Throwable {
        testAnimo("the a b c.");
        testAnimo("the x (a) (get b).");
        assertAnimoResult("x", "x (a b) (b c).");
    }

    @Test
    @Ignore
    public void test_39() throws Throwable {
        testAnimo("the a b c.");
        testAnimo("the x (a) (get b this a).");
        assertAnimoResult("x", "x (a b) (b c).");
    }

    @Test
    public void test_40() throws Throwable {
        testAnimo("the a (foo) (b c).");
        testAnimo("the y (z) (b v).");
        testAnimo("the x (a) (any z) (get b).");
        assertAnimoResult("any a", "the x (a (foo) (b)) (the y (z) (b)) (b c).");
    }

    @Test
    public void test_41() throws Throwable {
        testAnimo("the a (foo) (b c).");
        testAnimo("the y (z) (b v).");
        testAnimo("the x (a) (any z) (get b this a).");
        assertAnimoResult("any a", "the x (a (foo) (b)) (the y (z) (b)) (b c).");
    }

    @Test
    public void test_42() throws Throwable {
        testAnimo("the a (foo) (b c).");
        testAnimo("the x (any foo) (get b).");
        assertAnimoResult("x", "x (the a (foo) (b)) (b c).");
    }

    @Test
    public void test_43() throws Throwable {
        testAnimo("the a b c.");
        testAnimo("the c get d.");
        testAnimo("the x (get b a) (d e).");
        assertAnimoResult("x", "x (b c d e) (d).");
    }

    @Test
    public void test_44() throws Throwable {
        testAnimo("the a b c.");
        testAnimo("the c get d.");
        testAnimo("the x an (get b a) (d e).");
        assertAnimoResult("x", "x b c d e.");
    }

    @Test
    public void test_45() throws Throwable {
        testAnimo("the a get b.");
        assertAnimoResult("a b", "a b.");
    }

    @Test
    public void test_46() throws Throwable {
        testAnimo("the c b.");
        testAnimo("the a get b.");
        assertAnimoResult("a c", "a c.");
    }

    @Test
    public void test_47() throws Throwable {
        testAnimo("the c b.");
        testAnimo("the a each (get b) (this b).");
        assertAnimoResult("a (b) (c)", "a.");
    }

    @Test
    public void test_48() throws Throwable {
        testAnimo("the c b.");
        testAnimo("the a each (get b this a) (this b).");
        assertAnimoResult("a (b) (c)", "a.");
    }

    @Test
    public void test_49() throws Throwable {

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
    public void test_50() throws Throwable {

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
    public void test_51() throws Throwable {

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

    @Test
    public void test_000() throws Throwable {
        testAnimo("the a (x) (foo 1).");
        testAnimo("the y (a) (get foo).");
        assertAnimoResult("y", "y (a (x) (foo)) (foo 1).");
    }

    @Test
    public void test_001() throws Throwable {
        testAnimo("the a (x) (foo 1).");
        testAnimo("the y (any x) (get foo).");
        assertAnimoResult("y", "y (the a (x) (foo)) (foo 1).");
    }

    @Test
    public void test_002() throws Throwable {
        testAnimo("the a (x) (foo 1).");
        testAnimo("the y (all x) (get foo).");
        assertAnimoResult("y", "y (the a (x) (foo)) (foo 1).");
    }

    @Test
    public void test_003() throws Throwable {
        testAnimo("the a (x) (foo 1).");
        testAnimo("the y (prefer x use a) (get foo).");
        assertAnimoResult("y", "y (the a (x) (foo)) (foo 1).");
    }

    @Test
    public void test_004() throws Throwable {
        testAnimo("the a (x) (foo 1).");
        testAnimo("the y get foo.");
        testAnimo("the z y any x.");
        assertAnimoResult("z", "z y foo 1.");
    }

    @Test
    public void test_005() throws Throwable {
        testAnimo("the a x foo.");
        testAnimo("the b x bar.");
        testAnimo("the c (a, b) (get x).");
        assertAnimoResult("any a", "the c (a x) (b x) (x foo).");
        assertAnimoResult("any b", "the c (a x) (b x) (x bar).");
    }

}
