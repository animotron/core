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
package org.animotron.graph.builder;

import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.animotron.expression.JExpression;
import org.animotron.graph.serializer.AnimoSerializer;
import org.animotron.graph.serializer.DigestSerializer;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.animotron.statement.relation.IS;
import org.junit.Assert;
import org.junit.Test;

import static org.animotron.Properties.HASH;
import static org.animotron.expression.JExpression._;
import static org.animotron.graph.Cache.key;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class BindTest extends ATest {

    public void test(Expression e, String animo) throws Exception {
        String inA = AnimoSerializer.serialize(e);
        Object inH = key(HASH.get(e));
        Assert.assertEquals(inH, key(DigestSerializer.serialize(e)));
        cleanDb();
        Expression x = new AnimoExpression(animo);
        String outA = AnimoSerializer.serialize(x);
        Object outH = key(HASH.get(x));
        Assert.assertEquals(outH, key(DigestSerializer.serialize(x)));
        Assert.assertEquals(inH, outH);
        Assert.assertEquals(inA, outA);
    }

    @Test
	public void test_00() throws Exception {
        Expression a = new JExpression(
            _(AN._, "a")
        );
        Expression b = new JExpression(new StreamGraphBuilder(),
            _(THE._, "b", _(a))
        );
        test(b, "the b a");
	}

    @Test
	public void test_01() throws Exception {
        Expression a = new JExpression(
            _(AN._, "a")
        );
        Expression b = new JExpression(
            _(THE._, "b", _(a))
        );
        test(b, "the b a");
	}

    @Test
	public void test_02() throws Exception {
        Expression a = new JExpression(
            _(AN._, "a")
        );
        Expression b = new JExpression(new StreamGraphBuilder(),
            _(THE._, "b", _(IS._, "x"), _(a))
        );
        test(b, "the b (is x) (a)");
	}

    @Test
	public void test_03() throws Exception {
        Expression a = new JExpression(
            _(AN._, "a")
        );
        Expression b = new JExpression(
            _(THE._, "b", _(IS._, "x"), _(a))
        );
        test(b, "the b (is x) (a)");
	}

}
