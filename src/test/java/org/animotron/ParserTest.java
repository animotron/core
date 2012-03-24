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
package org.animotron;

import org.junit.Ignore;
import org.junit.Test;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ParserTest extends ATest {

    @Test
	public void test_00() throws Throwable {
    	testAnimo("the a.");
	}

    @Test
	public void test_01() throws Throwable {
    	testAnimo("the a b.");
	}

    @Test
	public void test_02() throws Throwable {
    	testAnimo("the a b \"test\".");
	}

    @Test
	public void test_03() throws Throwable {
    	testAnimo("the a (b) (c).");
	}

    @Test
	public void test_04() throws Throwable {
    	testAnimo("the a (b (any x) (all y)) (c).");
	}

    @Test
	public void test_05() throws Throwable {
    	testAnimo("the a \"b c\".");
	}

    @Test
    @Ignore
	public void test_06() throws Throwable {
    	testAnimo("the a the b", "the a b.");
	}

    @Test
    public void test_07() throws Throwable {
    	testAnimo("the a \\ get element-name.");
    }

    @Test
    public void test_08() throws Throwable {
    	testAnimo("the a \\ b.");
    }

    @Test
    public void test_09() throws Throwable {
    	testAnimo("the a \\ an b.", "the a \\ b.");
    }

    @Test
	public void test_10() throws Throwable {
    	testAnimo("the a \\ \"b\"", "the a \\ \"b\".");
	}

    @Test
	public void test_11() throws Throwable {
    	testAnimo("the a \\b.");
	}

    @Test
    public void test_12() throws Throwable {
    	testAnimo("the a @b \"c\".");
    }

    @Test
    public void test_13() throws Throwable {
    	testAnimo("the a @b (get c)", "the a @b get c.");
    }

    @Test
    public void test_14() throws Throwable {
    	testAnimo("the a @b get c.");
    }

    @Test
    public void test_15() throws Throwable {
    	testAnimo("the a @b get c d.");
    }

    @Test
    public void test_16() throws Throwable {
    	testAnimo("the a @b (get c) (all d).");
    }

    @Test
    public void test_17() throws Throwable {
    	testAnimo("the a @ (get c) (all d).");
    }

    @Test
    public void test_18() throws Throwable {
    	testAnimo("the a @b (get c) (all d).");
    }

    @Test
    public void test_19() throws Throwable {
    	testAnimo("the a @b (get c x) (all d y).");
    }

    @Test
    public void test_20() throws Throwable {
    	testAnimo("the a @(b c)", "the a @ b c.");
    }

    @Test
    public void test_21() throws Throwable {
    	testAnimo("the a @((b) (c))", "the a @ ((b) (c)).");
    }

    @Test
    public void test_22() throws Throwable {
    	testAnimo("the a @(\"b\" c)", "the a @ \"b\" (c).");
    }

    @Test
    public void test_23() throws Throwable {
    	testAnimo("the a @((\"b\") (c))", "the a @ (\"b\" (c)).");
    }

    @Test
    public void test_24() throws Throwable {
    	testAnimo("the a @ \"b\" (c) (d).");
    }

    @Test
    public void test_25() throws Throwable {
    	testAnimo("the a @x \"x\".");
    }

    @Test
    public void test_26() throws Throwable {
    	testAnimo("the a \"\\\"\".");
    }

    @Test
    public void test_27() throws Throwable {
    	testAnimo("the a ??stylesheet.");
    }

    @Test
    public void test_28() throws Throwable {
    	testAnimo("the a ??stylesheet b.");
    }

    @Test
    public void test_29() throws Throwable {
    	testAnimo("the a $b.");
    }

    @Test
    public void test_30() throws Throwable {
    	testAnimo("the a $b c.");
    }

    @Test
    public void test_31() throws Throwable {
    	testAnimo("the a !! \"html\".");
    }

    @Test
    public void test_32() throws Throwable {
    	testAnimo("the a &#amp.");
    }

    @Test
    public void test_33() throws Throwable {
    	testAnimo("the a b.");
    }

    @Test
    public void test_34() throws Throwable {
    	testAnimo("the a (b) (c) (d) (e).");
    }

    @Test
    public void test_35() throws Throwable {
    	testAnimo("the a (--> (b) (c)) (--> (d) (e))", "the a ((b) (c)) ((d) (e)).");
    }

    @Test
    public void test_36() throws Throwable {
    	testAnimo("the a ((b) (c)) ((d) (e)).");
    }

    @Test
    public void test_37() throws Throwable {
    	testAnimo("the a \"b\" (c).");
    }

    @Test
    public void test_38() throws Throwable {
    	testAnimo("the a (\"b\" c)", "the a \"b\" (c).");
    }

    @Test
    public void test_39() throws Throwable {
    	testAnimo("the a (b (c d))", "the a b c d.");
    }

    @Test
    public void test_40() throws Throwable {
    	testAnimo("the a ((b (c d)))", "the a (b c d).");
    }

    @Test
    public void test_41() throws Throwable {
    	testAnimo("the a (b (c (d)))", "the a b c d.");
    }

    @Test
    public void test_42() throws Throwable {
    	testAnimo("the a b.");
    }

    @Test
    public void test_43() throws Throwable {
    	testAnimo("the a (b)", "the a b.");
    }

    @Test
    public void test_44() throws Throwable {
    	testAnimo("the a ((b))", "the a (b).");
    }

    @Test
    public void test_45() throws Throwable {
    	testAnimo("the a (((b)))", "the a ((b)).");
    }

    @Test
    public void test_46() throws Throwable {
    	testAnimo("the a ((((b))))", "the a (((b))).");
    }

    @Test
    public void test_47() throws Throwable {
    	testAnimo("the a (--> b)", "the a (b).");
    }

    @Test
    public void test_48() throws Throwable {
    	testAnimo("the a (--> (b))", "the a (b).");
    }

    @Test
    public void test_49() throws Throwable {
    	testAnimo("the a (--> --> b)", "the a ((b)).");
    }

    @Test
    public void test_50() throws Throwable {
    	testAnimo("the a (--> --> (b))", "the a ((b)).");
    }

    @Test
    public void test_51() throws Throwable {
    	testAnimo("the a (--> (--> b))", "the a ((b)).");
    }

    @Test
    public void test_52() throws Throwable {
    	testAnimo("the a (--> (--> (b)))", "the a ((b)).");
    }

    @Test
    public void test_53() throws Throwable {
    	testAnimo("the a ((--> (b)))", "the a ((b)).");
    }

    @Test
    public void test_54() throws Throwable {
    	testAnimo("the a (--> ((b)))", "the a ((b)).");
    }

    @Test
    public void test_55() throws Throwable {
    	testAnimo("the a (--> (((b))))", "the a (((b))).");
    }

    @Test
    public void test_56() throws Throwable {
    	testAnimo("the a ((--> ((b))))", "the a (((b))).");
    }

    @Test
    public void test_57() throws Throwable {
    	testAnimo("the a (((--> (b))))", "the a (((b))).");
    }

    @Test
    public void test_58() throws Throwable {
    	testAnimo("the a ((((--> b))))", "the a ((((b)))).");
    }

    @Test
    public void test_59() throws Throwable {
    	testAnimo("the a (((--> (b)(c))))", "the a ((((b) (c)))).");
    }

    @Test
    public void test_60() throws Throwable {
    	testAnimo("the a (((--> (b)(c))(d)))", "the a ((((b) (c)) (d))).");
    }

    @Test
    public void test_61() throws Throwable {
    	testAnimo("the a (((--> (b)(c))(d)(e)))", "the a ((((b) (c)) (d) (e))).");
    }

    @Test
    public void test_62() throws Throwable {
    	testAnimo("the a (((--> (b)(c))(d)(e))(f))", "the a ((((b) (c)) (d) (e)) (f)).");
    }

    @Test
    public void test_63() throws Throwable {
    	testAnimo("the a (((--> (b)(c))(d)(e))(f)(g))", "the a ((((b) (c)) (d) (e)) (f) (g)).");
    }

    @Test
    public void test_64() throws Throwable {
    	testAnimo("the a (b) c", "the a (b) (c).");
    }

    @Test
    public void test_65() throws Throwable {
    	testAnimo("the a (b)c", "the a (b) (c).");
    }

    @Test
    public void test_66() throws Throwable {
    	testAnimo("the a b (c)", "the a b c.");
    }

    @Test
    public void test_67() throws Throwable {
    	testAnimo("the a b(c)", "the a b c.");
    }

    @Test
    public void test_68() throws Throwable {
    	testAnimo("the a(b(c))", "the a b c.");
    }

    @Test
    public void test_69() throws Throwable {
    	testAnimo("the a ((b))", "the a (b).");
    }

    @Test
    public void test_70() throws Throwable {
    	testAnimo("the a((b))", "the a (b).");
    }

    @Test
    public void test_71() throws Throwable {
    	testAnimo("the a((b)c)", "the a ((b) (c)).");
    }

    @Test
    public void test_72() throws Throwable {
    	testAnimo("the a ((b)c)", "the a ((b) (c)).");
    }

    @Test
    public void test_73() throws Throwable {
    	testAnimo("the a (((--> (b)c)(d)e)(f)g)", "the a ((((b) (c)) (d) (e)) (f) (g)).");
    }

    @Test
    public void test_74() throws Throwable {
    	testAnimo("the a (b c)", "the a b c.");
    }

    @Test
    public void test_75() throws Throwable {
    	testAnimo("the a ((b)(c))", "the a ((b) (c)).");
    }

    @Test
    public void test_76() throws Throwable {
    	testAnimo("the a (-->(b)(c))", "the a ((b) (c)).");
    }

    @Test
    public void test_77() throws Throwable {
    	testAnimo("the a (x (b)(c))", "the a x (b) (c).");
    }

    @Test
    public void test_78() throws Throwable {
    	testAnimo("the a (x(b)(c))", "the a x (b) (c).");
    }

    @Test
    public void test_79() throws Throwable {
    	testAnimo("the a (--> x (b)(c))", "the a (x (b) (c)).");
    }

    @Test
    public void test_80() throws Throwable {
    	testAnimo("the a (--> x(b)(c))", "the a (x (b) (c)).");
    }

    @Test
    public void test_81() throws Throwable {
    	testAnimo("the a (((b)(c)))", "the a (((b) (c))).");
    }

    @Test
    public void test_82() throws Throwable {
    	testAnimo("the a ((x (b)(c)))", "the a (x (b) (c)).");
    }

    @Test
    public void test_83() throws Throwable {
    	testAnimo("the a ((x(b)(c)))", "the a (x (b) (c)).");
    }

    @Test
    public void test_84() throws Throwable {
    	testAnimo("the a (x ((--> (b)c)(d)e)(f)g)", "the a x (((b) (c)) (d) (e)) (f) (g).");
    }

    @Test
    public void test_85() throws Throwable {
    	testAnimo("the a (x((--> (b)c)(d)e)(f)g)", "the a x (((b) (c)) (d) (e)) (f) (g).");
    }

    @Test
    public void test_86() throws Throwable {
    	testAnimo("the a (b) (c \"x\") (c \"x\") \"c\".");
    }

    @Test
    public void test_87() throws Throwable {
    	testAnimo("the a (b \"x\") (c \"x\").");
    }

    @Test
    public void test_88() throws Throwable {
    	testAnimo("\\a ($a \"a\") (@a \"a\") \"a\".");
    }

    @Test
    public void test_89() throws Throwable {
        testAnimo("the a.b.c.");
    }

    @Test
    public void test_90() throws Throwable {
        testAnimo("the a.b.c..", "the a.b.c.");
    }

    @Test
    public void test_91() throws Throwable {
        testAnimo("the get a.b.c any x.y.z.");
    }

    @Test
    public void test_92() throws Throwable {
        testAnimo("the get (a.x) (b.y) (c.z).");
    }

    @Test
    public void test_93() throws Throwable {
        testAnimo("the get (a.x)(b.y)(c.z)", "the get (a.x) (b.y) (c.z).");
    }

    @Test
    public void test_94() throws Throwable {
        testAnimo("the get a.x, b.y, c.z.");
    }

    @Test
    public void test_95() throws Throwable {
        testAnimo("the get a.x,b.y,c.z", "the get a.x, b.y, c.z.");
    }

    @Test
    public void test_96() throws Throwable {
        testAnimo("an get x.");
    }

    @Test
    public void test_97() throws Throwable {
        testAnimo("an (get x) (y).");
    }

    @Test
    public void test_98() throws Throwable {
        testAnimo("the z an (get x) (y).");
    }

}