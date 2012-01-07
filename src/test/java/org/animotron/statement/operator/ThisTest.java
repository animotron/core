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
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ThisTest extends ATest {

    @Test
    @Ignore //empty this
    public void test_00() throws Exception {
        testAnimo("the file uri id this.");
        assertAnimoResult("get uri file.", "uri \"file\".");
    }

    @Test
    @Ignore //empty this
    public void test_01() throws Exception {
        testAnimo("the file uri id this.");
        testAnimo("the x file.");
        assertAnimoResult("get uri x.", "uri \"x\".");
    }

    @Test
    @Ignore //empty this
    public void test_02() throws Exception {
        testAnimo("the file uri id this.");
        testAnimo("the x file.");
        testAnimo("the y file.");
        assertAnimoResult("get uri x.", "uri \"x\".");
        assertAnimoResult("get uri y.", "uri \"y\".");
    }

}
