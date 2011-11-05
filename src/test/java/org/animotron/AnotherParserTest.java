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

import org.junit.Test;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnotherParserTest extends ATest {

    @Test
	public void test_00() throws Exception {
        testAnimo("the a get b c");
	}

    @Test
	public void test_01() throws Exception {
        testAnimo("the a get an b an c");
	}

    @Test
	public void test_02() throws Exception {
        testAnimo("the a get an b an c");
	}

    @Test
	public void test_03() throws Exception {
        testAnimo("the a get (b) (c)");
	}

    @Test
	public void test_04() throws Exception {
        testAnimo("the a an (all b) (all c)");
	}

    @Test
	public void test_05() throws Exception {
        testAnimo("the a an b");
	}

    @Test
	public void test_06() throws Exception {
        testAnimo("an a b c");
	}

    @Test
	public void test_07() throws Exception {
        testAnimo("a b c");
	}

    @Test
    public void test_08() throws Exception {
    	testAnimo("get (get a) (an b)");
    }

    @Test
    public void test_09() throws Exception {
    	testAnimo("all a,b ((c,d) (e))");
    }

    @Test
    public void test_10() throws Exception {
		testAnimo("all a,b an c,d e");
    }

    @Test
    public void test_11() throws Exception {
		testAnimo("all a,b c,d e");
    }

    @Test
    public void test_12() throws Exception {
		testAnimo("all a,b c,d (e) (f)");
    }

}