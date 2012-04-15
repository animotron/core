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

import org.junit.Test;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class RevTest extends ATest {

    @Test
    public void test_00() throws Throwable {
        testAnimo("the a x.");
        testAnimo("the a y.");
        testAnimo("the a x.");
    }

    @Test
    public void test_01() throws Throwable {
        testAnimo("the a x.");
        testAnimo("the a y.");
        testAnimo("the a z.");
        testAnimo("the a y.");
        testAnimo("the a x.");
    }

    @Test
    public void test_02() throws Throwable {
        testAnimo("the a x.");
        testAnimo("the b x.");
        testAnimo("the a y.");
        testAnimo("the b y.");
        testAnimo("the a z.");
        testAnimo("the b z.");
    }

    @Test
    public void test_03() throws Throwable {
        testAnimo("the a x.");
        testAnimo("the a y.");
        testAnimo("the a z.");
        testAnimo("the a y.");
        testAnimo("the a x.");
        testAnimo("the b x.");
        testAnimo("the b y.");
        testAnimo("the b z.");
        testAnimo("the b y.");
        testAnimo("the b x.");
    }

}
