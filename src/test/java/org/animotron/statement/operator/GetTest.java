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
                _(DEF._, "A", _(AN._, "Z", value("A")))
            ),
            new JExpression(
                _(DEF._, "B", _(AN._, "Z", value("B")))
            )
        );

    	JExpression test = new JExpression(
			_(GET._, "Z", _(AN._, "A"), _(AN._, "B"))
		);
    	assertAnimoResultOneStep(test, "\"A\".", "\"B\".");
	}

	@Test
	public void getFromPFlow_an_with_param() throws Throwable {

    	__(new JExpression(
            _(DEF._, "A", element("B" , _(GET._, "C")))
        ));
    	JExpression test = new JExpression(
			_(AN._, "A", _(AN._, "C", value(".")))
		);
    	assertAnimoResult(test, "A \\B \".\".");

    	__(new JExpression(
            _(DEF._, "A1", _(GET._, "B1"))
        ));

    	test = new JExpression(
			_(AN._, "A1", _(AN._, "B1", value(".")))
		);
    	assertAnimoResult(test, "A1 \".\".");
	}
	
	@Test
	public void getFromPFlow_cross_an_with_param() throws Throwable {

        __(
            new JExpression(
                _(DEF._, "A", _(AN._, "B", _(GET._, "C")))
            ),
            new JExpression(
                _(DEF._, "D", element("E", _(GET._, "B")))
            )
        );
    	
    	JExpression test = new JExpression(
			_(AN._, "D", _(AN._, "A", _(AN._, "C", value(":"))))
		);
    	assertAnimoResult(test, "D \\E \":\".");
	}

	@Test
	public void getFromPFlow_an_with_an() throws Throwable {

        __(
            new JExpression(
                _(DEF._, "A", element("B", _(GET._, "C")))
            ),
            new JExpression(
                _(DEF._, "D", _(AN._, "C", value(".")))
            )
        );

    	JExpression test = new JExpression(
			_(AN._, "A", _(AN._, "D"))
		);
    	assertAnimoResult(test, "A \\B \".\".");
	}
	
	@Test
	public void getFromPFlow_an_with_more_an() throws Throwable {

    	__(
            new JExpression(
                _(DEF._, "A", element("B", _(GET._, "C")))
            ),
            new JExpression(
                _(DEF._, "D", _(AN._, "C", value(".")))
            ),
            new JExpression(
                _(DEF._, "E", _(AN._, "C", value(":")))
            )
        );

    	JExpression test = new JExpression(
			_(DEF._, "F", _(AN._, "A", _(AN._, "D"), _(AN._, "E", _(AN._, "C", value("_")))))
		);
    	assertAnimoResult(test, "def F A \\B \".\" \"_\".");

	}
	
    @Test
    public void getFromPFlow_an_with_stack() throws Throwable {

        __(
            new JExpression(
                _(DEF._, "A", _(GET._, "X"))
            ),
            new JExpression(
                _(DEF._, "B", _(GET._, "Y"), _(AN._, "A"))
            ),
            new JExpression(
                _(DEF._, "C", _(GET._, "Z"), _(AN._, "B"))
            )
        );

        JExpression E = new JExpression(
            _(AN._, "C", _(AN._, "X", value("α")), _(AN._, "Y", value("β")), _(AN._, "Z", value("γ")))
        );
        assertAnimoResult(E, "C \"γ\" (B \"β\" (A \"α\")).");
    }

    @Test
    public void getFromPFlow_more_an_with_stack() throws Throwable {

        __(
            new JExpression(
                _(DEF._, "A", _(GET._, "X"), _(AN._, "Z", value("γ")))
            ),
            new JExpression(
                _(DEF._, "B", _(GET._, "Y"), _(AN._, "A"))
            ),
            new JExpression(
                _(DEF._, "C", _(GET._, "Z"))
            )
        );

        JExpression E = new JExpression(
            _(DEF._, "E", _(AN._, "C", _(AN._, "B", _(AN._, "B", value("β")))))
        );
        assertAnimoResult(E, "def E C \"γ\".");

    }

    @Test
    public void getFromPFlow_one_more_an_with_stack() throws Throwable {

        __(
            new JExpression(
                _(DEF._, "A", _(GET._, "X"), _(AN._, "Z", value("γ")))
            ),
            new JExpression(
                _(DEF._, "B", _(GET._, "Y"))
            ),
            new JExpression(
                _(DEF._, "C", _(GET._, "Z"))
            )
        );

        JExpression E = new JExpression(
            _(DEF._, "E", _(AN._, "C", _(AN._, "B", _(AN._, "A"))))
        );
        assertAnimoResult(E, "def E C Z \"γ\".");
    }

    @Test
    public void getFromPFlow_an_an_an() throws Throwable {

        __(
            new JExpression(
                _(DEF._, "A", _(AN._, "Y", _(GET._, "X")))
            ),
            new JExpression(
                _(DEF._, "B", _(AN._, "Z", _(GET._, "Y")))
            ),
            new JExpression(
                _(DEF._, "C", _(GET._, "Z"))
            )
        );

        JExpression D = new JExpression(
            _(DEF._, "D", _(AN._, "C", _(AN._, "B", _(AN._, "A", _(AN._, "X", value("."))))))
        );
        assertAnimoResult(D, "def D C Z Y X \".\".");
    }
    
    @Test
	public void test_01() throws Throwable {
        __(
            new AnimoExpression("def a (x 1) (y 2) (z 3)"),
            new AnimoExpression("def b (get x) (get y) (get z)")
        );
        Expression e = new AnimoExpression("b a");
        assertStringResult(e, "123");
	}

    @Test
	public void test_02() throws Throwable {
        __(
                new AnimoExpression("def a (x 1)"),
                new AnimoExpression("def b (x 2)"),
                new AnimoExpression("def c (x 3)")
        );
        Expression e = new AnimoExpression("get x (a) (b) (c)");
        assertAnimoResult(e, "1.", "2.", "3.");
	}

    @Test
	public void test_03() throws Throwable {
        __(
            new AnimoExpression("def a x 1"),
            new AnimoExpression("def b x 2"),
            new AnimoExpression("def c x 3")
        );
        Expression e = new AnimoExpression("get x a,b,c");
        assertAnimoResult(e, "1. 2. 3.");
	}
    
	@Test
	public void test_04() throws Throwable {
		testAnimo("def user1 (user) (name \"user1\").");

		testAnimo("def item1 (item) (name \"item1\").");
		
		assertAnimoResult(
            new AnimoExpression("get name (user1) (item1)."),
            "\"user1\".", "\"item1\"."
        );

		assertAnimoResult(
            new AnimoExpression("get name (user1, item1)."),
            "\"user1\".", "\"item1\"."
        );

		assertAnimoResult(
            new AnimoExpression("get name user1,item1."),
            "\"user1\".", "\"item1\"."
        );
	}

	@Test
	@Ignore
	public void test_05() throws Throwable {
		testAnimo("def site host \"localhost\".");
		
		testAnimo("def resource get host.");
		
		assertAnimoResult("an site, resource", "site host. resource host \"localhost\".");
		
		assertAnimoResult("an resource, site", "resource host \"localhost\". site host.");
	}

	@Test
	public void test_06() throws Throwable {
		testAnimo("def file (extension \"txt\") (any mime-type with extension get extension).");
		
		testAnimo("def text-plain (mime-type) (text) (type \"text/plain\") (name \"Plain text\") (extension \"txt\" \"text\").");
		testAnimo("def text-html (mime-type) (text) (type \"text/html\") (name \"HTML Document\") (extension \"htm\" \"html\").");
		
		//assertAnimoResultOneStep("any mime-type with extension \"txt\"", "def text-plain (mime-type) (text) (type \"text/plain\") (name \"Plain text\") (extension \"txt\" \"text\").");

		assertAnimoResultOneStep("get mime-type file", "def text-plain (mime-type) (text) (type \"text/plain\") (name \"Plain text\") (extension \"txt\" \"text\").");
	}

    @Test
    public void test_07() throws Throwable {
        testAnimo("def male sex.");
        testAnimo("def john male.");

        assertAnimoResult("get sex john", "male.");
        assertAnimoResult("any male", "def john male sex.");
        assertAnimoResult("get sex any male", "male.");
    }

    @Test
    public void test_08() throws Throwable {
        testAnimo("def male sex.");
        testAnimo("def man male.");
        testAnimo("def john man.");

        assertAnimoResult("get sex john", "male.");
    }

    @Test
    @Ignore
    public void test_09() throws Throwable {
        testAnimo("def male #sex.");
        testAnimo("def john male.");

        testAnimo("def _man_ john.");
        assertAnimoResult("get sex _man_", "male.");

        testAnimo("def man any male.");

        assertAnimoResult("get sex man", "male.");
    }

    @Test
    @Ignore
    public void test_10() throws Throwable {
        testAnimo("def male sex.");
        testAnimo("def john person, male.");

        assertAnimoResult("any person", "def john (person) (male sex).");
        assertAnimoResult("get sex any person", "person, male.");
    }

    @Test
    @Ignore
    public void test_11() throws Throwable {
        testAnimo("def male sex.");
        testAnimo("def john person, male.");
        testAnimo("def man any person.");

        assertAnimoResult("get sex man", "person, male.");
    }

    @Test
    public void test_12() throws Throwable {
        testAnimo("def x b, c 1.");
        assertAnimoResult("get c, b x", "1.");
        assertAnimoResult("get c x", "1.");
        assertAnimoResult("get b x", "1.");
    }

    @Test
    public void test_13() throws Throwable {
        testAnimo("def a b c.");
        testAnimo("def x get b.");
        assertAnimoResult("x ^a", "x c.");
    }

    @Test
    public void test_14() throws Throwable {
        testAnimo("def a b c.");
        testAnimo("def x get b.");
        assertAnimoResult("x b get b a", "x c.");//UNDERSTAND: is it correct?
    }

    @Test
    public void test_15() throws Throwable {
        testAnimo("def a b c.");
        testAnimo("def x get b.");
        assertAnimoResult("x get b a", "x c.");
    }

    @Test
    public void test_16() throws Throwable {
        testAnimo("def x (foo) (bar).");
        testAnimo("def y ^x.");
        assertAnimoResult("get foo y", "x.");
    }

    @Test
    public void test_17() throws Throwable {
        testAnimo("def x bar.");
        assertAnimoResult("get bar x", "bar.");
    }

    @Test
    public void test_18() throws Throwable {
        testAnimo("def x foo z bar y.");
        assertAnimoResult("get bar x", "y.");
    }

    @Test
    public void test_19() throws Throwable {
        testAnimo("def x a 1.");
        testAnimo("def y a 2 (x).");
        assertAnimoResult("get a y", "2. x.");
    }

    @Test
    public void test_20() throws Throwable {
        testAnimo("def x a 1.");
        testAnimo("def y a 2 (x).");
        assertAnimoResult("each (get a y) (this a)", "2. x.");
    }

    @Test
    public void test_21() throws Throwable {
        testAnimo("def x a 1.");
        testAnimo("def y a 2 (b x).");
        assertAnimoResult("get a y", "2. b.");
    }

    @Test
    public void test_22() throws Throwable {
        testAnimo("def x a 1.");
        testAnimo("def y a 2 (b x).");
        assertAnimoResult("each (get a y) (this a)", "2. b.");
    }

    @Test
    public void test_23() throws Throwable {
        testAnimo("def x a z.");
        testAnimo("def y a 2 (b c x).");
        assertAnimoResult("get a y", "2. b.");
    }

    @Test
    public void test_24() throws Throwable {
        testAnimo("def x a z.");
        testAnimo("def y a 2 (b c x).");
        assertAnimoResult("each (get a y) (this a)", "2. b.");
    }

    @Test
    public void test_25() throws Throwable {
        testAnimo("def x \\a y z.");
        assertAnimoResult("get y x", "");
    }

    @Test
    public void test_26() throws Throwable {
        testAnimo("def x y z.");
        testAnimo("def z \\bar.");
        assertAnimoResult("def foo an (get y x)", "def foo z \\bar.");
    }

    @Test
    public void test_27() throws Throwable {
        testAnimo("def x y z.");
        testAnimo("def z \\bar.");
        assertAnimoResult("def foo (^x) (an (get y))", "def foo (x y) (z \\bar).");
    }

    @Test
    public void test_28() throws Throwable {
        testAnimo("def x y z.");
        testAnimo("def z get bar.");
        testAnimo("def a bar 0.");
        assertAnimoResult("def foo (^a) (^x) (an (get y))", "def foo (a bar) (x y) (z 0).");
    }

    @Test
    public void test_29() throws Throwable {
        testAnimo("def foo bar.");
        testAnimo("def x y ^foo.");
        assertAnimoResult("get foo x", "foo.");
        assertAnimoResult("get bar x", "foo.");
        assertAnimoResult("get foo get y x", "foo.");
        assertAnimoResult("get bar get y x", "bar.");
    }

    @Test
    public void test_29_() throws Throwable {
        testAnimo("def bar 1.");
        testAnimo("def x y foo bar.");
        assertAnimoResult("get foo x", "bar."); 
    }

    @Test
    public void test_29__() throws Throwable {
        testAnimo("def bar 1.");
        testAnimo("def x y foo bar.");
        assertAnimoResult("get foo get y x", "bar.");
    }

    @Test
    @Ignore
    //XXX: this looks very wrong...
    public void test_30() throws Throwable {
        testAnimo("def x y foo.");
        assertAnimoResult("get foo x", "foo.");
        assertAnimoResult("get foo get y x", "foo.");
        assertAnimoResult("def q1 (x) (get foo)", "def q1 (x y) (y foo).");
        assertAnimoResult("def q2 (x) (get foo get y)", "def q2 (x y) (y sfoo).");
        assertAnimoResult("def q3 (x) (an get foo)", "def q3 (x y) (foo).");
        assertAnimoResult("def q4 (x) (an get foo get y)", "def q4 (x y) (foo).");
    }

    @Test
    @Ignore
    //XXX: this looks veeeeery wrong...
    public void test_30_() throws Throwable {
        testAnimo("def x y foo.");
        assertAnimoResult("get foo x", "foo.");
        assertAnimoResult("get foo get y x", "foo.");
        assertAnimoResult("def q1 (x) (\\p get foo)", "def q1 (x y) (\\p foo).");
        assertAnimoResult("def q2 (x) (\\p get foo get y)", "def q2 (x y) (\\p foo).");
        assertAnimoResult("def q3 (x) (\\p an get foo)", "def q3 (x y) (\\p foo).");
        assertAnimoResult("def q4 (x) (\\p an get foo get y)", "def q4 (x y) (\\p foo).");
    }

    @Test
    public void test_31() throws Throwable {
        testAnimo("def bar z.");
        testAnimo("def foo1 (bar) 1.");
        testAnimo("def foo2 (bar) 2.");
        testAnimo("def x y foo1.");
    }

    @Test
    public void test_32() throws Throwable {
        testAnimo("def bar z.");
        testAnimo("def foo1 (bar) 1.");
        testAnimo("def foo2 (bar) 2.");
        testAnimo("def x y (^foo3) (^foo1).");
        assertAnimoResult("get bar x", "foo1.");
    }

    @Test
    public void test_33() throws Throwable {
        testAnimo("def bar z.");
        testAnimo("def foo1 (bar) 1.");
        testAnimo("def foo2 (bar) 2.");
        assertAnimoResult("get bar get y x", "");//"foo1."
        //XXX: answer '' correct because getDef y x return ''
    }

    @Test
    public void test_34() throws Throwable {
        testAnimo("def bar z.");
        testAnimo("def foo1 (bar) (get a).");
        testAnimo("def foo2 (bar) 2.");
        //assertAnimoResult("foo1 a 1", "foo1 (bar z) (a 1).");
        assertAnimoResult("an (an foo1) (a 1)", "foo1 (bar z) 1.");
        //assertAnimoResult("an (getDef bar getDef y x) (a 1)", "foo1 a 1."); //answer '' correct because (getDef bar getDef y x) == ''
    }

    @Test
    public void test_34_() throws Throwable {
        testAnimo("def bar z.");
        testAnimo("def foo1 (bar) (\\p get a).");
        testAnimo("def foo2 (bar) 2.");
        assertAnimoResult("foo1 a 1", "foo1 (bar z) (\\p 1).");
        assertAnimoResult("an (an foo1) (a 1)", "foo1 (bar z) (\\p 1).");
        //assertAnimoResult("an (getDef bar getDef y x) (a 1)", "foo1 \\p a 1."); //answer '' correct because (getDef bar getDef y x) == ''
    }

    @Test
    public void test_35() throws Throwable {
        testAnimo("def bar z.");
        testAnimo("def foo1 (bar) 1.");
        testAnimo("def foo2 (bar) 2.");
        testAnimo("def x y ^foo3, foo1.");
        assertAnimoResult("get bar x", "foo3. foo1."); //UNDERSTAND: foo3, foo1?
    }

    @Test
    @Ignore //parser not ready
    public void test_36() throws Throwable {
        testAnimo("def bar z.");
        testAnimo("def foo1 (bar) 1.");
        testAnimo("def foo2 (bar) 2.");
        testAnimo("def x y ^foo3, foo1.");
        assertAnimoResult("get bar get y x", "foo3, foo1.");
    }

    @Test
    public void test_37() throws Throwable {
        testAnimo("def x a b.");
        testAnimo("def y (x) (a c).");
        assertAnimoResult("get a y", "c.");
        //assertAnimoResult("getDef b getDef a y", "a b.");
    }

    @Test
    public void test_38() throws Throwable {
        testAnimo("def a b c.");
        testAnimo("def x (^a) (get b).");
        assertAnimoResult("x", "x (a b) (c).");
    }

    @Test
    @Ignore
    public void test_39() throws Throwable {
        testAnimo("def a b c.");
        testAnimo("def x (a) (get b this a).");
        assertAnimoResult("x", "x (a b) (b c).");
    }

    @Test
    public void test_40() throws Throwable {
        testAnimo("def a (foo) (b c).");
        testAnimo("def y (z) (b v).");
        testAnimo("def x (^a) (any z) (get b).");
        assertAnimoResult("any a", "def x (a (foo) (b)) (def y (z) (b)) (c).");
    }

    @Test
    public void test_41() throws Throwable {
        testAnimo("def a (foo) (b c).");
        testAnimo("def y (z) (b v).");
        testAnimo("def x (^a) (any z) (get b this a).");
        assertAnimoResult("any a", "def x (a (foo) (b)) (def y (z) (b)) (c).");
    }

    @Test
    @Ignore // a is not on vector
    public void test_42() throws Throwable {
        testAnimo("def a (foo) (b c).");
        testAnimo("def x (any foo) (get b).");
        assertAnimoResult("x", "x (def a (foo) (b)) (b c).");
    }

    @Test
    public void test_43() throws Throwable {
        testAnimo("def a b c.");
        testAnimo("def c get d.");
        testAnimo("def x (get b a) (d e).");
        assertAnimoResult("x", "x (c d e) (d).");
    }

    @Test
    public void test_44() throws Throwable {
        testAnimo("def a b c.");
        testAnimo("def c get d.");
        testAnimo("def x an (get b a) (d e).");
        assertAnimoResult("x", "x c e.");
    }

    @Test
    public void test_45() throws Throwable {
        testAnimo("def a get b.");
        assertAnimoResult("a b", "a b.");
    }

    @Test
    public void test_46() throws Throwable {
        testAnimo("def c b.");
        testAnimo("def a get b.");
        assertAnimoResult("a ^c", "a c.");
    }

    @Test
    public void test_47() throws Throwable {
        testAnimo("def c b.");
        testAnimo("def a each (get b) (this b).");
        assertAnimoResult("a (b) (c)", "a b.");
    }

    @Test
    public void test_48() throws Throwable {
        testAnimo("def c b.");
        testAnimo("def a each (get b this a) (this b).");
        assertAnimoResult("a (b) (c)", "a b.");
    }

    @Test
    public void test_49() throws Throwable {

        __(
            new JExpression(
                    _(DEF._, "B", _(NONSTOP._, "A"))
            ),
            new JExpression(
                    _(DEF._, "C", _(AN._, "B", value("π")))
            )
        );

        JExpression E = new JExpression(
            _(DEF._, "E", _(GET._, "A", _(AN._, "C")))
        );
        assertAnimoResult(E, "def E \"π\".");

    }

    @Test
    @Ignore //no SHALL
    public void test_50() throws Throwable {

        __(
            new JExpression(
                    _(DEF._, "B", _(AN._, "A"))
            ),
            new JExpression(
                    _(DEF._, "C", _(SHALL._, "B", value("π")))
            ),
            new JExpression(
                    _(DEF._, "D", _(AN._, "C"))
            )
        );

        JExpression E = new JExpression(
            _(DEF._, "E", _(GET._, "A", _(AN._, "D")))
        );
        //XXX: assertAnimoResult(E, "def E B \"π\".");
        assertAnimoResult(E, "def E shall B \"π\".");
    }

    @Test
    public void test_51() throws Throwable {

        __(
            new JExpression(
                    _(DEF._, "B", _(NONSTOP._, "A"))
            ),
            new JExpression(
                    _(DEF._, "C", _(NONSTOP._, "B", value("π")))
            ),
            new JExpression(
                    _(DEF._, "D", _(NONSTOP._, "C"))
            )
        );

        JExpression E = new JExpression(
            _(DEF._, "E", _(GET._, "A", _(AN._, "D")))
        );
        assertAnimoResult(E, "def E \"π\".");
    }

    @Test
    public void test_000() throws Throwable {
        testAnimo("def a (x) (foo 1).");
        testAnimo("def y (a) (get foo).");
        assertAnimoResult("y", "y (a (x) (foo)) 1.");
    }

    @Test
    @Ignore // "foo 1" don't stay on vector (need to calc "any x" to reach)
    public void test_001() throws Throwable {
        testAnimo("def a (x) (foo 1).");
        testAnimo("def y (any x) (get foo).");
        assertAnimoResult("y", "y (def a (x) (foo)) (foo 1).");
    }

    @Test
    @Ignore // "foo 1" don't stay on vector (need to calc "all x" to reach)
    public void test_002() throws Throwable {
        testAnimo("def a (x) (foo 1).");
        testAnimo("def y (all x) (get foo).");
        assertAnimoResult("y", "y (def a (x) (foo)) (foo 1).");
    }

    @Test
    @Ignore // "foo 1" don't stay on vector (need to calc "prefer x" to reach)
    public void test_003() throws Throwable {
        testAnimo("def a (x) (foo 1).");
        testAnimo("def y (prefer x use a) (get foo).");
        assertAnimoResult("y", "y (def a (x) (foo)) (foo 1).");
    }

    @Test
    @Ignore // "foo 1" don't stay on vector (need to calc "any x" to reach)
    public void test_004() throws Throwable {
        testAnimo("def a (x) (foo 1).");
        testAnimo("def y get foo.");
        testAnimo("def z y any x.");
        assertAnimoResult("z", "z y foo 1.");
    }

}
