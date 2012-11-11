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
package org.animotron.graph.builder;

import com.ctc.wstx.stax.WstxInputFactory;
import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.animotron.expression.JExpression;
import org.animotron.expression.StAXExpression;
import org.animotron.graph.serializer.CachedSerializer;
import org.animotron.graph.serializer.DigestSerializer;
import org.animotron.statement.ml.ELEMENT;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.ASHIFT;
import org.animotron.statement.operator.DEF;
import org.animotron.statement.query.ANY;
import org.animotron.statement.query.GET;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import org.neo4j.graphdb.Direction;

import javax.xml.stream.XMLInputFactory;
import java.io.StringReader;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.element;
import static org.animotron.graph.AnimoGraph.startDB;
import static org.animotron.graph.Properties.HASH;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class BindTest extends ATest {

    @Test
	public void test_00() throws Throwable {
        Expression a = new JExpression(
            _(AN._, "a")
        );
        Expression b = new JExpression(
            _(DEF._, "b", _(a))
        );
        assertAnimo(b, "def b a.");
	}

    @Test
	public void test_01() throws Throwable {
        Expression a = new JExpression(
            _(AN._, "a")
        );
        Expression b = new JExpression(
            _(DEF._, "b", _(a))
        );
        assertAnimo(b, "def b a.");
	}

    @Test
	public void test_02() throws Throwable {
        Expression a = new JExpression(
            _(AN._, "a")
        );
        Expression b = new JExpression(
            _(DEF._, "b", _(AN._, "x"), _(a))
        );
        assertAnimo(b, "def b (x) (a).");
	}

    @Test
	public void test_03() throws Throwable {
        Expression a = new JExpression(
            _(AN._, "a")
        );
        Expression b = new JExpression(
            _(DEF._, "b", _(AN._, "x"), _(a))
        );
        assertAnimo(b, "def b (x) (a).");
	}

    @Test
	public void test_04() throws Throwable {
        Expression x = new JExpression(
            _(DEF._, "x")
        );
        Expression y = new JExpression(
            _(DEF._, "y")
        );
        Expression b = new JExpression(
            _(DEF._, "b", _(AN._, x), _(AN._, y))
        );
        assertAnimo(b, "def b (x) (y).");
	}

    @Test
	public void test_05() throws Throwable {
        Expression a = new JExpression(
            _(ANY._, "a")
        );
        Expression b = new JExpression(
            _(DEF._, "b", _(AN._, "y", _(a)))
        );
        assertAnimo(b, "def b y any a.");
	}

    @Test
	public void test_06() throws Throwable {
        Expression a = new JExpression(
            _(ANY._, "a")
        );
        Expression x = new JExpression(
            _(DEF._, "x")
        );
        Expression y = new JExpression(
            _(DEF._, "y")
        );
        Expression b = new JExpression(
            _(DEF._, "b", _(AN._, x), _(AN._, y, _(a)))
        );
        assertAnimo(b, "def b (x) (y any a).");
	}

}
