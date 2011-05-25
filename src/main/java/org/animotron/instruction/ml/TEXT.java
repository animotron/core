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

import org.animotron.Properties;
import org.animotron.instruction.AbstractInstruction;
import org.neo4j.graphdb.Node;


/**
 * Instruction 'ml:text'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class TEXT extends AbstractInstruction {
	
	private static final TEXT INSTANCE = new TEXT();
	public static TEXT getInstance() { return INSTANCE; }
	
	private TEXT() { super("text", "ml", "animo/ml"); }

	protected TEXT(String name) { super(name, "ml", "animo/ml"); }

	public Node build (Node parent, String value){
		Node node = super.build(parent);
		Properties.VALUE.set(node, value);
		return node;
	}
	
}
