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

import java.io.File;
import java.io.InputStream;

import org.neo4j.graphdb.Relationship;

import com.ctc.wstx.stax.WstxInputFactory;



/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class BinGraphBuilder extends GraphBuilder {
	
	private final static WstxInputFactory INPUT_FACTORY = new WstxInputFactory();
	private final static File STORAGE = new File(AnimoGraph.STORAGE, "bin");
	
	private InputStream stream;
	private String path;
	
	public BinGraphBuilder(InputStream stream, String path) {
		this.stream = stream;
		this.path = path;
	}
	
	public Relationship build() {
		//TODO: Store binary and build graph for one
		return getRelationship();
	}

}
