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
package org.animotron.graph.serializer;

import org.animotron.ATest;
import org.animotron.expression.Expression;
import org.junit.Test;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnimoSerializerTest extends ATest {
	
	@Test
	public void test_00() throws Throwable {
        testAnimo("def A (X) (use Y).");
    }

    @Test
    public void test_01() throws Throwable {
        testAnimo("def A (X) (Y).");
    }

    @Test
    public void test_02() throws Throwable {
        testAnimo("def A X Y.");
    }

    @Test
    public void test_03() throws Throwable {
        testAnimo("def A (X) (Y).");
    }

    @Test
    public void test_04() throws Throwable {
        testAnimo("def A B C D \".\".");
    }

    @Test
    public void test_05() throws Throwable {
        testAnimo("def A B C (D) \".\".");
    }

    @Test
    public void test_06() throws Throwable {
        testAnimo("def A B C (D \".\") (E \"_\").");
    }

    @Test
    public void test_07() throws Throwable {
        testAnimo("def A (X) (B C (D \".\") (E \"_\")).");
    }

    @Test
    public void test_08() throws Throwable {
        testAnimo("def A (X) (B (C (D \".\") (E \"_\")) (F (G \":\") (H \";\"))).");
    }

    @Test
    public void test_09() throws Throwable {
        testAnimo("def A (X) (B (C (D \"1\") (E \"2\")) (F (G \"3\") (H \"4\"))) (I (J (K \"5\") (L \"6\")) (M (N \"7\") (O \"8\"))).");
	}

    @Test
    public void test_0A() throws Throwable {
        testAnimo("def A \"bla\" \"bla\".");
    }
	
    @Test
    public void test_0B() throws Throwable {
        Expression A = testAnimo("def A (B) (C).");
        assertAnimoResult(A, "A (B) (C).");
    }

    @Test
    public void test_0C() throws Throwable {
        testAnimo("def A ((B) (C)).");
    }

    @Test
    public void test_0D() throws Throwable {
        testAnimo("def A (B C).", "def A B C.");
    }

    @Test
    public void test_0E() throws Throwable {
        testAnimo("def A ((B) (C)) ((D) (E)).");
    }

    @Test
    public void test_0F() throws Throwable {
        testAnimo("def A B.");
    }

    @Test
    public void test_10() throws Throwable {
        testAnimo("def A (B).", "def A B.");
    }

    @Test
    public void test_11() throws Throwable {
        testAnimo("def A ((B)).", "def A (B).");
    }

    @Test
    public void test_12() throws Throwable {
        testAnimo("def A (((B))).", "def A ((B)).");
    }

    @Test
    public void test_13() throws Throwable {
        testAnimo("def A (B).", "def A B.");
    }

    @Test
    public void test_14() throws Throwable {
        testAnimo("def A ((B)).", "def A (B).");
    }

    @Test
    public void test_15() throws Throwable {
        testAnimo("def A (((B))).", "def A ((B)).");
    }

    @Test
    public void test_16() throws Throwable {
        testAnimo("def A ((B)).", "def A (B).");
    }

    @Test
    public void test_17() throws Throwable {
        testAnimo("def A ((B)).", "def A (B).");
    }
}
