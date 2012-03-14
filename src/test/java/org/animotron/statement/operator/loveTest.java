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
public class loveTest extends ATest {
	
    @Test
    public void test() throws Throwable {
        testAnimo("the Peter person.");
        testAnimo("the Kate person.");
        testAnimo("the Joe person.");
        testAnimo("the John person.");
        testAnimo("the Lily person.");
        testAnimo("the Lui person.");
        testAnimo("the event1 love (Peter, Kate) (date \"2012-03-14\").");
        testAnimo("the event2 love (John, Joe) (date sometime).");
        testAnimo("the event3 love (Joe, Lily) (date \"2012-03-12\").");
        testAnimo("the event4 love (Lily, Lui) (date \"2012-03-15\").");
        testAnimo("the today date \"2012-03-14\".");
        assertAnimoResult("get person all love.", "Kate. Joe. Lily. Lui.");
    }

}