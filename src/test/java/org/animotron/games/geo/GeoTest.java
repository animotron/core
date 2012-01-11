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
package org.animotron.games.geo;

import com.ctc.wstx.stax.WstxInputFactory;
import org.animotron.ATest;
import org.animotron.expression.StAXExpression;
import org.animotron.graph.builder.FastGraphBuilder;
import org.junit.Test;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import static org.animotron.graph.AnimoGraph.cleanDB;
import static org.animotron.graph.AnimoGraph.startDB;
import static org.animotron.graph.Properties.HASH;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class GeoTest extends ATest {

    private XMLInputFactory FACTORY = new WstxInputFactory();

    private static final String OSM = "/geo/map.osm";

    private XMLStreamReader osm() throws XMLStreamException {
        return osm(OSM);
    }

    private XMLStreamReader osm(String resource) throws XMLStreamException {
        return FACTORY.createXMLStreamReader(getClass().getResourceAsStream(resource));
    }

    @Test
	public void test_01() throws Exception {
        byte[] in = (byte[]) HASH.get(new StAXExpression(new FastGraphBuilder(), osm()));
        cleanDB(DATA_FOLDER);
        startDB(DATA_FOLDER);
        byte[] out = (byte[]) HASH.get(new StAXExpression(osm()));
        assertEquals(in, out);
	}

    @Test
	public void test_02() throws Exception {
        byte[] in = (byte[]) HASH.get(new StAXExpression(new FastGraphBuilder(), osm()));
        byte[] out = (byte[]) HASH.get(new StAXExpression(osm()));
        assertEquals(in, out);
	}

    @Test
	public void test_03() throws Exception {
        byte[] in = (byte[]) HASH.get(new StAXExpression(osm()));
        byte[] out = (byte[]) HASH.get(new StAXExpression(new FastGraphBuilder(), osm()));
        assertEquals(in, out);
	}

    @Test
	public void test() throws Exception {
        new StAXExpression(osm());
	}

}
