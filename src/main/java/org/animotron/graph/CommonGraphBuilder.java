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
package org.animotron.graph;

import java.io.InputStream;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.animotron.graph.stax.StAXGraphBuilder;
import org.neo4j.graphdb.Relationship;

import com.ctc.wstx.stax.WstxInputFactory;



/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class CommonGraphBuilder {
	
	private final static WstxInputFactory INPUT_FACTORY = new WstxInputFactory();
	
	private static XMLStreamReader createXMLStreamReader(InputStream stream) throws XMLStreamException {
		return INPUT_FACTORY.createXMLStreamReader(stream);
	}
	
	public static Relationship build(InputStream stream) throws XMLStreamException {
		return new StAXGraphBuilder(createXMLStreamReader(stream)).build();
	}
	
	public static Relationship Relationship(InputStream stream, String path) {
		try {
			return new StAXGraphBuilder(createXMLStreamReader(stream)).build();
		} catch (XMLStreamException e) {
			return new BinGraphBuilder(stream, path).build();
		}
	}

}
