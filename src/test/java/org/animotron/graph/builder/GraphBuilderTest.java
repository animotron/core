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

import junit.framework.Assert;
import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.graph.serializer.AnimoSerializer;
import org.junit.Test;

import static org.animotron.Properties.HASH;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class GraphBuilderTest extends ATest {

    private void test_0(String animo) throws Exception {
        AnimoExpression e;
        e = new AnimoExpression(new FastGraphBuilder(), animo);
        String inA = AnimoSerializer.serialize(e);
        byte[] inH = (byte[]) HASH.get(e);
        cleanDb();
        e = new AnimoExpression(new StreamGraphBuilder(), animo);
        String outA = AnimoSerializer.serialize(e);
        byte[] outH = (byte[]) HASH.get(e);
        assertEquals(inH, outH);
        Assert.assertEquals(inA, outA);
    }

    private void test_1(String animo) throws Exception {
        AnimoExpression e;
        e = new AnimoExpression(new StreamGraphBuilder(), animo);
        String outA = AnimoSerializer.serialize(e);
        byte[] outH = (byte[]) HASH.get(e);
        e = new AnimoExpression(new FastGraphBuilder(), animo);
        String inA = AnimoSerializer.serialize(e);
        byte[] inH = (byte[]) HASH.get(e);
        assertEquals(inH, outH);
        Assert.assertEquals(inA, outA);
    }

    private void test_2(String animo) throws Exception {
        AnimoExpression e;
        e = new AnimoExpression(new FastGraphBuilder(), animo);
        String inA = AnimoSerializer.serialize(e);
        byte[] inH = (byte[]) HASH.get(e);
        e = new AnimoExpression(new StreamGraphBuilder(), animo);
        String outA = AnimoSerializer.serialize(e);
        byte[] outH = (byte[]) HASH.get(e);
        assertEquals(inH, outH);
        Assert.assertEquals(inA, outA);
    }

    private void test(String animo) throws Exception {
        test_0(animo);
        cleanDb();
        test_1(animo);
        cleanDb();
        test_2(animo);
    }

    @Test
	public void test_00() throws Exception {
        test("\\a");
	}

    @Test
	public void test_01() throws Exception {
        test("\\x:a $x \"x-namespace\"");
	}

    @Test
	public void test_02() throws Exception {
        test("\\a $ \"x-namespace\"");
	}

    @Test
	public void test_03() throws Exception {
        test("\\a @b \"c\"");
	}

    @Test
	public void test_04() throws Exception {
        test("(??stylesheet \"path\") (\\a)");
	}

}
