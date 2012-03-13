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
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ParentChildTest extends ATest {
	
    @Test
    public void test_00() throws Throwable {
        testAnimo("= parent, child.");
        testAnimo("the Joe parent John.");
        assertAnimoResult("get parent Joe.", "parent John.");
        assertAnimoResult("get child John.", "child Joe.");
    }

    @Test
    public void test_01() throws Throwable {
        testAnimo("= parent, child.");
        testAnimo("= (son) (child male).");
        testAnimo("= (father) (parent male).");
        testAnimo("the Joe (male) (parent John.)");
        testAnimo("the John male.");
        assertAnimoResult("get parent Joe.", "parent John.");
        assertAnimoResult("get child John.", "child Joe.");
        assertAnimoResult("all father.", "the John male.");
        assertAnimoResult("all son.", "the Joe (male) (parent).");
    }

}