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
package org.animotron.bridge;

import org.animotron.ATest;
import org.animotron.exception.AnimoException;
import org.animotron.expression.BinaryExpression;
import org.animotron.graph.serializer.CachedSerializer;
import org.animotron.statement.operator.THE;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import static org.animotron.expression.AnimoExpression.__;
import static org.junit.Assert.assertNotNull;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class BridgeTest extends ATest {
	
	private void check(String the) throws IOException {
        Relationship r = THE._.getActual(the);
        assertNotNull(r);
        CachedSerializer.ANIMO.serialize(r, System.out);
        System.out.println();
	}
	
	@Test
	public void FSloadAndSerialize() throws Exception {
        System.out.println("Test repository loader ...");
        FSBridge._.load("src/test/animo/application-animo.animo");
        System.out.println("loaded ...");
        check("application-animo");
        System.out.println("done.");
	}

	@Test
	public void ZIPloadAndSerialize() throws Exception {
        System.out.println("Test repository loader ...");
        new ZipBridge().load("src/test/resources/test.zip");
        System.out.println("loaded ...");
        check("second.txt");
        System.out.println("done.");
	}

    private class ZipBridge extends AbstractZipBridge {

        @Override
        protected void loadEntry(ZipInputStream zis, final ZipEntry entry) {
            __(
                    new BinaryExpression(zis, false) {
                        @Override
                        protected String id () {
                            return entry.getName();
                        }
                        @Override
                        protected void description() throws AnimoException, IOException {}
                    }
            );
        }
    }
}