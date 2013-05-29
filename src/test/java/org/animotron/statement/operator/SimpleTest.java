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
package org.animotron.statement.operator;

import org.animotron.ATest;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class SimpleTest extends ATest {
	
	@Test
	public void an() throws Throwable {
        
    	__(
            "def AA.",
            "def BB AA 'a@b'."
        );

        assertAnimoResultOneStep("def C1 BB", "C1 BB AA \"a@b\".");

        assertAnimoResult("def C2 BB", "C2 BB AA.");// AA \"a@b\".");
	}

	@Test
	public void get() throws Throwable {

        __(
            "def A.",
            "def B A 'a@b'."
        );

        assertAnimoResult("def C get A B", "C \"a@b\".");
	}
}