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
package org.animotron.statement.ml;

import org.animotron.ATest;
import org.animotron.bridge.FSBridge;
import org.animotron.statement.operator.THE;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;

import java.io.File;

import static org.junit.Assert.assertNotNull;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class FormGeneratorTest extends ATest {

	@Test
	public void check() throws Exception {

		FSBridge._.load(new File("src/main/animo/form-generator.animo"), "/binary/");
        
        Relationship r = THE._.get("form-generator");
        
        assertNotNull(r);
        
        assertAnimoResult(r, "the form-generator THE.");
        
        //XXX: complete
	}
}
