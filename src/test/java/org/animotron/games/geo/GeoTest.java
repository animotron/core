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
import org.junit.Test;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

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
	public void test() throws Throwable {
        new StAXExpression(osm());
	}

}
