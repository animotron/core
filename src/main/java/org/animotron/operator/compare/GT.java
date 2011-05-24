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
package org.animotron.operator.compare;

import java.io.IOException;

import org.animotron.io.PipedOutputObjectStream;
import org.animotron.operator.AbstractOperator;
import org.neo4j.graphdb.Relationship;

/**
 * Compare operator 'GT'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class GT extends AbstractOperator {
	
	public static final GT INSTANCE = new GT();
	public static GT getInstance() { return INSTANCE; }
	
	private GT() { super("gt", "animo/compare/gt"); }

	@Override
	public void eval(Relationship op, PipedOutputObjectStream out, boolean isLast) throws IOException {
		//TODO: code
	}
}
