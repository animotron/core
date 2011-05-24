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
package org.animotron.instruction.perform;

import java.io.IOException;

import org.animotron.instruction.AbstractInstruction;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.Relationship;

/**
 * Instruction 'do:xsl', plug XSL language
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class XSL extends AbstractInstruction {
	
	public static final XSL INSTANCE = new XSL();
	public static XSL getInstance() { return INSTANCE; }
	
	private XSL() { super("xsl", "do", "animo/perform"); }

	@Override
	public void eval(Relationship op, PipedOutputObjectStream out, boolean isLast) throws IOException {
		//TODO: code
	}
}
