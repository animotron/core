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
import org.junit.Ignore;
import org.junit.Test;

import static org.animotron.expression.AnimoExpression.__;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class DefTest extends ATest {
	
    @Test
    @Ignore
    public void testDEF() throws Throwable {

        Expression A = new AnimoExpression("def A def B def C");
        assertAnimoResult(A, "def A the B the C.");
        assertAnimo(A, "def A the B the C.");
    }

	@Test
 	public void testREV() throws Throwable {
//        Expression e;
//        e = testAnimo("def e 1.");
//        String uuid1 = (String) UUID.getDef(e);
//        e = testAnimo("def e 2.");
//        String uuid2 = (String) UUID.getDef(e);
//        Relationship rev1 = e.getEndNode().getSingleRelationship(SHIFT, OUTGOING);
//        Assert.assertNotNull(rev1);
//        Assert.assertEquals("", UUID.getDef(rev1), uuid1);
//        e = testAnimo("def e 3.");
//        String uuid3 = (String) UUID.getDef(e);
//        Relationship rev2 = e.getEndNode().getSingleRelationship(SHIFT, OUTGOING).getEndNode().getSingleRelationship(SHIFT, OUTGOING);
//        Assert.assertNotNull(rev2);
//        Assert.assertEquals("", UUID.getDef(rev2), uuid1);
//        Assert.assertFalse(uuid1.equals(uuid2));
//        Assert.assertFalse(uuid2.equals(uuid3));
	}

	@Test
 	public void testREV_00() throws Throwable {
        testAnimo("def e 1.");
        testAnimo("def e 2.");
        testAnimo("def e 3.");
        testAnimo("def e 1.");
	}

	@Test
 	public void testREV_01() throws Throwable {
		testAnimo("def e name (lang-en \"name\") (lang-ru \"исправленное имя\").");

        testAnimo("def e name (lang-en \"corrected name\") (lang-ru \"исправленное имя\").");
	}
	
	@Test
 	public void testREV_02() throws Throwable {
		testAnimo("def e name (lang-en \"name\") (lang-ru \"исправленное имя\").");

        testAnimo("def e name (lang-en \"corrected name\") (lang-ru \"исправленное имя\").");

		testAnimo("def e name (lang-en \"name\") (lang-ru \"исправленное имя\").");
	}

	@Test
 	public void testREV_10() throws Throwable {
        testAnimo("def e 1.");
        testAnimo("def e 2.");
        assertAnimoResultOneStep("e", "e 2."); //UNDERSTAND: is it correct, missing DEF at beginning?

        testAnimo("def e 1.");
        assertAnimoResultOneStep("e", "e 1."); //UNDERSTAND: is it correct, missing DEF at beginning?
	}

    @Test
    public void test_11() throws Throwable {

        __(
	        "def item (goods aaa) (qty (1) (kg)) (cost (10) (USD))."
        );

        assertAnimoResult(
            "get cost item",
            "10. USD."
        );

        __(
	        "def item (goods aaa) (qty (1) (kg)) (cost (5) (USD))."
        );

        assertAnimoResult(
            "get cost item",
            "5. USD."
		);
    }
}