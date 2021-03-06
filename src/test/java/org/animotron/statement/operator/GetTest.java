/*
 *  Copyright (C) 2011-2013 The Animo Project
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
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class GetTest extends ATest {
	
	@Test
	public void getOnManyAN() throws Throwable {

        __("def A Z 'A'.");
        __("def B Z 'B'.");

    	assertAnimoResult("get Z (A) (B)", "\"A\".", "\"B\".");
	}

	@Test
	public void getFromPFlow_an_with_param() throws Throwable {

        __("def A get C.");

        assertAnimoResult("A C '.'", "A \".\".");

        __("def A1 get B1.");

    	assertAnimoResult("A1 B1 '.'", "A1 \".\".");
	}
	
	@Test
	public void getFromPFlow_cross_an_with_param() throws Throwable {

        __(
    		"def A B get C.",
    		"def D get B."
		);

    	assertAnimoResult("D A C ':'", "D \":\".");
	}

	@Test
	public void getFromPFlow_an_with_an() throws Throwable {

        __("def A get C.");
        __("def D C '.'.");

        assertAnimoResult("A D", "A \".\".");
	}
	
	@Test
	public void getFromPFlow_an_with_more_an() throws Throwable {

        __("def A get C.");
        __("def D C '.'.");
        __("def E C ':'.");

    	assertAnimoResult("def F A (D) (E C '_')", "F A \".\" \"_\".");

	}
	
    @Test
    public void getFromPFlow_an_with_stack() throws Throwable {

        __("def A get X.");
        __("def B (get Y) (A).");
        __("def C (get Z) (B).");

        assertAnimoResult("C (X 'α') (Y 'β') (Z 'γ')", "C \"γ\" (B \"β\" (A \"α\")).");
    }

    @Test
    public void getFromPFlow_more_an_with_stack() throws Throwable {

        __("def A (get X) (Z 'γ').");
        __("def B (get Y) (A).");
        __("def C get Z.");

        assertAnimoResult("def E C B B 'β'", "E C \"γ\".");

    }

    @Test
    public void getFromPFlow_one_more_an_with_stack() throws Throwable {

        __("def A (get X) (Z 'γ').");
        __("def B get Y.");
        __("def C get Z.");

        assertAnimoResult("def E C  B A", "E C \"γ\".");
    }

    @Test
    public void getFromPFlow_an_an_an() throws Throwable {

        __("def A Y get X.");
        __("def B Z get Y.");
        __("def C get Z.");

        assertAnimoResult("def D C B A X '.'", "D C \".\".");
    }
    
    @Test
	public void test_01() throws Throwable {
        __(
            "def a (x 1) (y 2) (z 3).",
            "def b (get x) (get y) (get z)."
        );

        assertStringResult("b a", "123");
	}

    @Test
	public void test_02() throws Throwable {
        __(
            "def a x 1.",
            "def b x 2.",
            "def c x 3."
        );

        assertAnimoResult("get x (a) (b) (c)", "1.", "2.", "3.");
	}

    @Test
	public void test_03() throws Throwable {
        __(
            "def a x 1.",
            "def b x 2.",
            "def c x 3."
        );

        assertAnimoResult("get x a,b,c", "1. 2. 3.");
	}
    
	@Test
	public void test_04() throws Throwable {
		testAnimo("def user1 (user) (name \"user1\").");

		testAnimo("def item1 (item) (name \"item1\").");
		
		assertAnimoResult(
            "get name (user1) (item1).",
            "\"user1\".", "\"item1\"."
        );

		assertAnimoResult(
            "get name (user1, item1).",
            "\"user1\".", "\"item1\"."
        );

		assertAnimoResult(
            "get name user1,item1.",
            "\"user1\".", "\"item1\"."
        );
	}

	@Test
	public void test_05() throws Throwable {
		testAnimo("def site host \"localhost\".");
		
		testAnimo("def resource get host.");
		
		assertAnimoResult("an site, resource", "site host. resource \"localhost\".");
		
		assertAnimoResult("an resource, site", "resource \"localhost\". site host.");
	}

	@Test
	public void test_06() throws Throwable {
		testAnimo("def file (extension \"txt\") (any mime-type with extension get extension).");
		
		testAnimo("def text-plain (mime-type) (text) (type \"text/plain\") (name \"Plain text\") (extension \"txt\" \"text\").");
		testAnimo("def text-html (mime-type) (text) (type \"text/html\") (name \"HTML Document\") (extension \"htm\" \"html\").");
		
		assertAnimoResultOneStep("any mime-type with extension \"txt\"", "text-plain (mime-type) (text) (type \"text/plain\") (name \"Plain text\") (extension \"txt\" \"text\").");

		assertAnimoResultOneStep("get mime-type file", "text-plain (mime-type) (text) (type \"text/plain\") (name \"Plain text\") (extension \"txt\" \"text\").");
	}

    @Test
    public void test_07() throws Throwable {
        testAnimo("def male sex.");
        testAnimo("def john male.");

        assertAnimoResult("get sex john", "male sex.");
        assertAnimoResult("any male", "john male sex.");
        assertAnimoResult("get sex any male", "male sex.");
    }

    @Test
    public void test_08() throws Throwable {
        testAnimo("def male sex.");
        testAnimo("def man male.");
        testAnimo("def john man.");

        assertAnimoResult("get sex john", "male sex.");
    }

    @Test
    @Ignore
    public void test_09() throws Throwable {
        testAnimo("def male sex.");
        testAnimo("def john male.");

        testAnimo("def _man_ john.");
        assertAnimoResult("get sex _man_", "male sex.");

        testAnimo("def man any male.");

        assertAnimoResult("get sex man", "male sex.");
    }

    @Test
    public void test_10() throws Throwable {
        testAnimo("def male sex.");
        testAnimo("def john person, male.");

        assertAnimoResult("any person", "john (person) (male sex).");
        assertAnimoResult("get sex any person", "person. male sex.");
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
        assertAnimoResult("get foo y", "x (foo) (bar).");
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
        assertAnimoResult("get a y", "2. x a.");
    }

    @Test
    public void test_20() throws Throwable {
        testAnimo("def x a 1.");
        testAnimo("def y a 2 (x).");
        assertAnimoResult("each (get a y) (this a)", "2. x a.");
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
        testAnimo("def x any a y z.");
        assertAnimoResult("get y x", "");
    }

    @Test
    public void test_26() throws Throwable {
        testAnimo("def x y z.");
        testAnimo("def z bar.");
        assertAnimoResult("def foo an (get y x)", "foo z bar.");
    }

    @Test
    public void test_27() throws Throwable {
        testAnimo("def x y z.");
        testAnimo("def z bar.");
        assertAnimoResult("def foo (^x) (get y)", "foo (x y) (z bar).");

        assertAnimoResult("def foo (^x) (an (get y))", "foo (x y) (z bar).");
    }

    @Test
    public void test_28() throws Throwable {
        testAnimo("def x y z.");
        testAnimo("def z get bar.");
        testAnimo("def a bar 0.");
        assertAnimoResult("def foo (^a) (^x) (an (get y))", "foo (a bar) (x y) (z 0).");
    }

    @Test
    public void test_29() throws Throwable {
        testAnimo("def foo bar.");
        testAnimo("def x y ^foo.");
        assertAnimoResult("get foo x", "foo bar.");
        assertAnimoResult("get bar x", "foo bar.");
        assertAnimoResult("get foo get y x", "foo bar.");
        assertAnimoResult("get bar get y x", "bar.");
    }

    @Test
    public void test_29_() throws Throwable {
        testAnimo("def bar 1.");
        testAnimo("def x y foo bar.");
        assertAnimoResult("get foo x", "bar 1."); 
    }

    @Test
    public void test_29__() throws Throwable {
        testAnimo("def bar 1.");
        testAnimo("def x y foo bar.");
        assertAnimoResult("get foo get y x", "bar 1.");
    }

    @Test
    @Ignore
    //XXX: this looks very wrong...
    public void test_30() throws Throwable {
        testAnimo("def x y foo.");
        assertAnimoResult("get foo x", "foo.");
        assertAnimoResult("get foo get y x", "foo.");
        assertAnimoResult("def q1 (x) (get foo)", "q1 (x y) (y foo).");
        assertAnimoResult("def q2 (x) (get foo get y)", "q2 (x y) (y sfoo).");
        assertAnimoResult("def q3 (x) (an get foo)", "q3 (x y) (foo).");
        assertAnimoResult("def q4 (x) (an get foo get y)", "q4 (x y) (foo).");
    }

    @Test
    @Ignore
    //XXX: this looks veeeeery wrong...
    public void test_30_() throws Throwable {
        testAnimo("def x y foo.");
        assertAnimoResult("get foo x", "foo.");
        assertAnimoResult("get foo get y x", "foo.");
        assertAnimoResult("def q1 (x) (p get foo)", "q1 (x y) (p foo).");
        assertAnimoResult("def q2 (x) (p get foo get y)", "q2 (x y) (p foo).");
        assertAnimoResult("def q3 (x) (p an get foo)", "q3 (x y) (p foo).");
        assertAnimoResult("def q4 (x) (p an get foo get y)", "q4 (x y) (p foo).");
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
        assertAnimoResult("get bar x", "foo1 (bar z) 1.");
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
        __(
    		"def bar z.",
    		"def foo1 (bar) (p get a).",
    		"def foo2 (bar) 2.",
    		"def p get 1."
		);
        assertAnimoResult("foo1 a 1", "foo1 (bar z) (p 1).");
        assertAnimoResult("an (an foo1) (a 1)", "foo1 (bar z) (p 1).");
        //assertAnimoResult("an (getDef bar getDef y x) (a 1)", "foo1 p a 1."); //answer '' correct because (getDef bar getDef y x) == ''
    }

    @Test
    public void test_35() throws Throwable {
        testAnimo("def bar z.");
        testAnimo("def foo1 (bar) 1.");
        testAnimo("def foo2 (bar) 2.");
        testAnimo("def x y ^foo3, foo1.");
        assertAnimoResult("get bar x", "foo3. foo1 (bar z) 1."); //UNDERSTAND: foo3, foo1?
    }

    @Test
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
    @Ignore //UNDERSTAND: require pseudoIS topology check during THIS evaluation. do we really need it?
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
        assertAnimoResult("any a", "x (a (foo) (b)) (y (z) (b)) (c).");
    }

    @Test
    public void test_41() throws Throwable {
        testAnimo("def a (foo) (b c).");
        testAnimo("def y (z) (b v).");
        testAnimo("def x (^a) (any z) (get b this a).");
        assertAnimoResult("any a", "x (a (foo) (b)) (y (z) (b)) (c).");
    }

    @Test
    @Ignore
    public void test_42() throws Throwable {
    }

    @Test
    public void test_43() throws Throwable {
        testAnimo("def a b c.");
        testAnimo("def c get d.");
        testAnimo("def x (get b a) (d e).");
        assertAnimoResult("x", "x (c e) (d).");
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
        assertAnimoResult("a ^c", "a c b.");
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

        __("def B A.");
        __("def C B 'π'.");
        Expression E = new AnimoExpression("def E get A C");
        assertAnimoResult(E, "E \"π\".");

    }

    @Test
    @Ignore //no SHALL
    public void test_50() throws Throwable {

        __("def B A.");
        __("def C shall B 'π'.");
        __("def D C.");
        Expression E = new AnimoExpression("def E get A D");
        //XXX: assertAnimoResult(E, "E B \"π\".");
        assertAnimoResult(E, "E shall B \"π\".");
    }

    @Test
    public void test_51() throws Throwable {

        __("def B A.");
        __("def C B 'π'.");
        __("def D ^C.");
        Expression E = new AnimoExpression("def E get A D");
        assertAnimoResult(E, "E \"π\".");
    }

    @Test
    public void test_000() throws Throwable {
        testAnimo("def a (x) (foo 1).");
        testAnimo("def y (^a) (get foo).");
        assertAnimoResult("y", "y (a (x) (foo)) 1.");
    }
}