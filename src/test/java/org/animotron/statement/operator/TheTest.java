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
import org.animotron.expression.Expression;
import org.animotron.expression.JExpression;
import org.junit.Ignore;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.graph.Properties.UUID;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class TheTest extends ATest {
	
    @Test
    @Ignore
    public void testTHE() throws Exception {

        JExpression A = new JExpression(
            _(THE._, "A", _(THE._, "B", _(THE._, "C")))
        );
        assertAnimoResult(A, "the A the B the C.");
        assertAnimo(A, "the A the B the C.");
    }

	@Test
 	public void testREV() throws Exception {
//        Expression e;
//        e = testAnimo("the e 1.");
//        String uuid1 = (String) UUID.get(e);
//        e = testAnimo("the e 2.");
//        String uuid2 = (String) UUID.get(e);
//        Relationship rev1 = e.getEndNode().getSingleRelationship(REV, OUTGOING);
//        Assert.assertNotNull(rev1);
//        Assert.assertEquals("", UUID.get(rev1), uuid1);
//        e = testAnimo("the e 3.");
//        String uuid3 = (String) UUID.get(e);
//        Relationship rev2 = e.getEndNode().getSingleRelationship(REV, OUTGOING).getEndNode().getSingleRelationship(REV, OUTGOING);
//        Assert.assertNotNull(rev2);
//        Assert.assertEquals("", UUID.get(rev2), uuid1);
//        Assert.assertFalse(uuid1.equals(uuid2));
//        Assert.assertFalse(uuid2.equals(uuid3));
	}

	@Test
 	public void testREV_00() throws Exception {
        testAnimo("the e 1.");
        testAnimo("the e 2.");
        testAnimo("the e 3.");
        testAnimo("the e 1.");
	}

	@Test
 	public void testREV_01() throws Exception {
		Expression e;
		
		e = testAnimo("the e name (lang-en \"name\") (lang-ru \"исправленное имя\").");
        System.out.println(UUID.get(e));
		
        e = testAnimo("the e name (lang-en \"corrected name\") (lang-ru \"исправленное имя\").");
        System.out.println(UUID.get(e));
	}
	
	@Test
 	public void testREV_02() throws Exception {
		Expression e;
		
		e = testAnimo("the e name (lang-en \"name\") (lang-ru \"исправленное имя\").");
        System.out.println(UUID.get(e));
		
        e = testAnimo("the e name (lang-en \"corrected name\") (lang-ru \"исправленное имя\").");
        System.out.println(UUID.get(e));

		e = testAnimo("the e name (lang-en \"name\") (lang-ru \"исправленное имя\").");
        System.out.println(UUID.get(e));
	}

	@Test
 	public void testREV_10() throws Exception {
        testAnimo("the e 1.");
        testAnimo("the e 2.");
        assertAnimoResultOneStep("e", "e 2."); //UNDERSTAND: is it correct, missing THE at beginning?

        testAnimo("the e 1.");
        assertAnimoResultOneStep("e", "e 1."); //UNDERSTAND: is it correct, missing THE at beginning?
	}
}