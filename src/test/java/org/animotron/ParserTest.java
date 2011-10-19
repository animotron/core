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
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron;

import org.animotron.expression.AnimoExpression;
import org.animotron.graph.serializer.DigestSerializer;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import static org.animotron.Properties.HASH;
import static org.animotron.graph.Cache.key;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ParserTest extends ATest {

    private void test(String exp) throws Exception {
        test(exp, exp);
    }

    private void test(String in, String out) throws Exception {
        AnimoExpression expression = new AnimoExpression(in);
        Assert.assertEquals(HASH.get(expression), key(DigestSerializer.serialize(expression)));
        assertAnimo(expression, out);
    }

    @Test
	public void test_00() throws Exception {
        test("the a");
	}

    @Test
	public void test_01() throws Exception {
        test("the a have b");
	}

    @Test
	public void test_02() throws Exception {
        test("the a have b \"test\"");
	}

    @Test
	public void test_03() throws Exception {
        test("the a (have b) (have c)");
	}

    @Test
	public void test_04() throws Exception {
        test("the a (have b (any x) (all y)) (have c)");
	}

    @Test
	public void test_05() throws Exception {
        test("the a \"b c\"");
	}

    @Test
    @Ignore
	public void test_06() throws Exception {
        test("the a the b", "the a b");
	}

    @Test
    public void test_07() throws Exception {
        test("the a \\ get element-name");
    }

    @Test
    public void test_08() throws Exception {
        test("the a \\ b");
    }

    @Test
    public void test_09() throws Exception {
        test("the a \\ an b", "the a \\ b");
    }

    @Test
	public void test_10() throws Exception {
        test("the a \\ \"b\"", "the a \\b");
	}

    @Test
	public void test_11() throws Exception {
        test("the a \\b");
	}

    @Test
    public void test_12() throws Exception {
        test("the a @b \"c\"");
    }

    @Test
    public void test_13() throws Exception {
        test("the a @b (get c)", "the a @b get c");
    }

    @Test
    public void test_14() throws Exception {
        test("the a @b get c");
    }

    @Test
    public void test_15() throws Exception {
        test("the a @b get c d");
    }

    @Test
    public void test_16() throws Exception {
        test("the a @b (get c) (all d)");
    }

    @Test
    public void test_17() throws Exception {
        test("the a @ (get c) (all d)");
    }

    @Test
    public void test_18() throws Exception {
        test("the a @b (get c) (all d)");
    }

    @Test
    public void test_19() throws Exception {
        test("the a @b (get c x) (all d y)");
    }

    @Test
    public void test_20() throws Exception {
        test("the a @(b c)", "the a @ b c");
    }

    @Test
    public void test_21() throws Exception {
        test("the a @((b) (c))", "the a @ ((b) (c))");
    }

    @Test
    public void test_22() throws Exception {
        test("the a @(\"b\" c)", "the a @ \"b\" c");
    }

    @Test
    public void test_23() throws Exception {
        test("the a @((\"b\") (c))", "the a @ ((\"b\") (c))");
    }

    @Test
    public void test_24() throws Exception {
        test("the a @ ((\"b\") (c) (d))");
    }

    @Test
    public void test_25() throws Exception {
        test("the a @x \"x\"");
    }

    @Test
    public void test_26() throws Exception {
        test("the a \"\\\"\"");
    }

    @Test
    public void test_27() throws Exception {
        test("the a ??stylesheet");
    }

    @Test
    public void test_28() throws Exception {
        test("the a ??stylesheet b");
    }

    @Test
    public void test_29() throws Exception {
        test("the a $b");
    }

    @Test
    public void test_30() throws Exception {
        test("the a $b c");
    }

    @Test
    public void test_31() throws Exception {
        test("the a !! \"html\"");
    }

    @Test
    public void test_32() throws Exception {
        test("the a &#amp");
    }

    @Test
    public void test_33() throws Exception {
        test("the a is b");
    }

    @Test
    public void test_34() throws Exception {
        test("the a (b) (c) (d) (e)");
    }

    @Test
    public void test_35() throws Exception {
        test("the a (--> (b) (c)) (--> (d) (e))", "the a ((b) (c)) ((d) (e))");
    }

    @Test
    public void test_36() throws Exception {
        test("the a ((b) (c)) ((d) (e))");
    }

    @Test
    public void test_37() throws Exception {
        test("the a \"b\" c");
    }

    @Test
    public void test_38() throws Exception {
        test("the a (\"b\" c)", "the a \"b\" c");
    }

    @Test
    public void test_39() throws Exception {
        test("the a (b (c d))", "the a b c d");
    }

    @Test
    public void test_40() throws Exception {
        test("the a ((b (c d)))", "the a (b c d)");
    }

    @Test
    public void test_41() throws Exception {
        test("the a (b (c (d)))", "the a b c d");
    }

    @Test
    public void test_42() throws Exception {
        test("the a b");
    }

    @Test
    public void test_43() throws Exception {
        test("the a (b)", "the a b");
    }

    @Test
    public void test_44() throws Exception {
        test("the a ((b))", "the a (b)");
    }

    @Test
    public void test_45() throws Exception {
        test("the a (((b)))", "the a ((b))");
    }

    @Test
    public void test_46() throws Exception {
        test("the a ((((b))))", "the a (((b)))");
    }

    @Test
    public void test_47() throws Exception {
        test("the a (--> b)", "the a (b)");
    }

    @Test
    public void test_48() throws Exception {
        test("the a (--> (b))", "the a (b)");
    }

    @Test
    public void test_49() throws Exception {
        test("the a (--> --> b)", "the a ((b))");
    }

    @Test
    public void test_50() throws Exception {
        test("the a (--> --> (b))", "the a ((b))");
    }

    @Test
    public void test_51() throws Exception {
        test("the a (--> (--> b))", "the a ((b))");
    }

    @Test
    public void test_52() throws Exception {
        test("the a (--> (--> (b)))", "the a ((b))");
    }

    @Test
    public void test_53() throws Exception {
        test("the a ((--> (b)))", "the a ((b))");
    }

    @Test
    public void test_54() throws Exception {
        test("the a (--> ((b)))", "the a ((b))");
    }

    @Test
    public void test_55() throws Exception {
        test("the a (--> (((b))))", "the a (((b)))");
    }

    @Test
    public void test_56() throws Exception {
        test("the a ((--> ((b))))", "the a (((b)))");
    }

    @Test
    public void test_57() throws Exception {
        test("the a (((--> (b))))", "the a (((b)))");
    }

    @Test
    public void test_58() throws Exception {
        test("the a ((((--> b))))", "the a ((((b))))");
    }

    @Test
    public void test_59() throws Exception {
        test("the a (((--> (b)(c))))", "the a ((((b) (c))))");
    }

    @Test
    public void test_60() throws Exception {
        test("the a (((--> (b)(c))(d)))", "the a ((((b) (c)) (d)))");
    }

    @Test
    public void test_61() throws Exception {
        test("the a (((--> (b)(c))(d)(e)))", "the a ((((b) (c)) (d) (e)))");
    }

    @Test
    public void test_62() throws Exception {
        test("the a (((--> (b)(c))(d)(e))(f))", "the a ((((b) (c)) (d) (e)) (f))");
    }

    @Test
    public void test_63() throws Exception {
        test("the a (((--> (b)(c))(d)(e))(f)(g))", "the a ((((b) (c)) (d) (e)) (f) (g))");
    }

    @Test
    public void test_64() throws Exception {
        test("the a (b) c", "the a (b) (c)");
    }

    @Test
    public void test_65() throws Exception {
        test("the a (b)c", "the a (b) (c)");
    }

    @Test
    public void test_66() throws Exception {
        test("the a b (c)", "the a b c");
    }

    @Test
    public void test_67() throws Exception {
        test("the a b(c)", "the a b c");
    }

    @Test
    public void test_68() throws Exception {
        test("the a(b(c))", "the a b c");
    }

    @Test
    public void test_69() throws Exception {
        test("the a ((b))", "the a (b)");
    }

    @Test
    public void test_70() throws Exception {
        test("the a((b))", "the a (b)");
    }

    @Test
    public void test_71() throws Exception {
        test("the a((b)c)", "the a ((b) (c))");
    }

    @Test
    public void test_72() throws Exception {
        test("the a ((b)c)", "the a ((b) (c))");
    }

    @Test
    public void test_73() throws Exception {
        test("the a (((--> (b)c)(d)e)(f)g)", "the a ((((b) (c)) (d) (e)) (f) (g))");
    }

    @Test
    public void test_74() throws Exception {
        test("the a (b c)", "the a b c");
    }

    @Test
    public void test_75() throws Exception {
        test("the a ((b)(c))", "the a ((b) (c))");
    }

    @Test
    public void test_76() throws Exception {
        test("the a (-->(b)(c))", "the a ((b) (c))");
    }

    @Test
    public void test_77() throws Exception {
        test("the a (x (b)(c))", "the a x (b) (c)");
    }

    @Test
    public void test_78() throws Exception {
        test("the a (x(b)(c))", "the a x (b) (c)");
    }

    @Test
    public void test_79() throws Exception {
        test("the a (--> x (b)(c))", "the a (x (b) (c))");
    }

    @Test
    public void test_80() throws Exception {
        test("the a (--> x(b)(c))", "the a (x (b) (c))");
    }

    @Test
    public void test_81() throws Exception {
        test("the a (((b)(c)))", "the a (((b) (c)))");
    }

    @Test
    public void test_82() throws Exception {
        test("the a ((x (b)(c)))", "the a (x (b) (c))");
    }

    @Test
    public void test_83() throws Exception {
        test("the a ((x(b)(c)))", "the a (x (b) (c))");
    }

    @Test
    public void test_84() throws Exception {
        test("the a (x ((--> (b)c)(d)e)(f)g)", "the a x (((b) (c)) (d) (e)) (f) (g)");
    }

    @Test
    public void test_85() throws Exception {
        test("the a (x((--> (b)c)(d)e)(f)g)", "the a x (((b) (c)) (d) (e)) (f) (g)");
    }

    @Test
    public void test_86() throws Exception {
        test("the a (is b) (have c \"x\") (have c \"x\") (\"c\")");
    }

    @Test
    public void test_87() throws Exception {
        test("the a (b \"x\") (c \"x\")");
    }

    @Test
    public void test_88() throws Exception {
        test("\\a ($a \"a\") (@a \"a\") (\"a\")");
    }

}
