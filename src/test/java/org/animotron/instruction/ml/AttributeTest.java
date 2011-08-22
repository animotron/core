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
package org.animotron.instruction.ml;

import static org.junit.Assert.assertNotNull;

import java.io.File;

import org.animotron.ATest;
import org.animotron.graph.builder.CommonBuilder;
import org.animotron.graph.serializer.GraphSerializer;
import org.animotron.operator.THE;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AttributeTest extends ATest {

	@Test
	public void check() throws Exception {

		CommonBuilder.build(new File("src/main/animo/form-generator.animo"));
        
        Relationship r = THE._.get("form-generator");
        
        assertNotNull(r);
        
        System.out.println("outputing ....");
        GraphSerializer.serialize(r, System.out);
        
        //assertAnimo(r, "<the:C>z</the:C>");
	}
}