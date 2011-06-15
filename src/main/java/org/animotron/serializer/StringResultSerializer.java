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
package org.animotron.serializer;

import org.animotron.Statement;
import org.animotron.instruction.ml.TEXT;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class StringResultSerializer extends AbstractResultSerializer {
	
	private StringBuilder builder;
	private String result;
	
	public String getString() {
		return result;
	}

	/* (non-Javadoc)
	 * @see org.animotron.graph.GraphHandler#start(org.animotron.Statement, org.neo4j.graphdb.Relationship)
	 */
//	@Override
	public void start(Statement statement, Relationship r) {
		//System.out.println(r);
		if (statement instanceof TEXT) {
			System.out.println("TEXT found");
			builder.append(statement.value(r));
		} else {
			//build(r);
		}
	}

	/* (non-Javadoc)
	 * @see org.animotron.graph.GraphHandler#end(org.animotron.Statement, org.neo4j.graphdb.Relationship)
	 */
//	@Override
	public void end(Statement statement, Relationship r) {
	}

	/* (non-Javadoc)
	 * @see org.animotron.graph.GraphHandler#startDocument()
	 */
//	@Override
	public void startDocument() {
		builder = new StringBuilder(1024);
		result = null;
	}

	/* (non-Javadoc)
	 * @see org.animotron.graph.GraphHandler#endDocument()
	 */
//	@Override
	public void endDocument() {
		result = builder.toString();
		builder = null;
	}
}