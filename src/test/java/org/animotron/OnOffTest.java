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
package org.animotron;

import junit.framework.Assert;
import org.animotron.expression.Expression;
import org.junit.Test;

import static org.animotron.graph.AnimoGraph.*;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class OnOffTest extends ATest {

    @Test
	public void test() throws Throwable {

        Expression e = testAnimo("def a b.");

        long id = e.getId();

        assertAnimoResult("a", "a b.");

        shutdownDB();
        startDB(DATA_FOLDER);

        Assert.assertNotNull(getDb().getNodeById(id));

        assertAnimoResult("a", "a b.");

    }

}