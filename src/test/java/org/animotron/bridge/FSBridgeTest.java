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
package org.animotron.bridge;

import static org.junit.Assert.assertNotNull;

import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import org.animotron.ATest;
import org.animotron.graph.GraphSerializer;
import org.animotron.operator.THE;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdobsky</a>
 *
 */
public class FSBridgeTest extends ATest {
	
	private static final String PATH = "src/main/animo/mime/application-animo.animo";
	
	private void check(String the) throws XMLStreamException{
        Relationship r = THE._.get(the);
        assertNotNull(r);
        GraphSerializer.serialize(r, System.out);
        System.out.println();
	}
	
	@Test
	public void loadAndSerialize() throws XMLStreamException, IOException {
        System.out.println("Test repository loader ...");
        FSBridge.load(PATH);
        System.out.println("loaded ...");
        check("application-animo");
        System.out.println("done.");
	}
}
