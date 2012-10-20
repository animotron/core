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
package org.animotron.games.web;

import org.animotron.ATest;
import org.junit.Test;

/**                   n
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class HtmlTest extends ATest {

    @Test
    public void test_00() throws Throwable {
        tAnimo("def el '<' (id this el) '>' (get 1) '</' (id this el) '>'.");
        tAnimo("def p el.");
        assertStringResult("p 'para'", "<p>para</p>");
    }

    @Test
    public void test_01() throws Throwable {
        tAnimo("def el '<' (id this el) '>' (get 1) '</' (id this el) '>'.");
        tAnimo("def ul el.");
        tAnimo("def li el.");
        assertStringResult("ul li 1", "<ul><li>1</li></ul>");
    }

}
