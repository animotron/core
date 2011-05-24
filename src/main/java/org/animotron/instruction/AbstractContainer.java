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
package org.animotron.instruction;

import java.util.Map;

import javolution.util.FastMap;

import org.animotron.instruction.Instruction;
import org.animotron.instruction.InstructionContainer;

/**
 * Abstract instructions container.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public abstract class AbstractContainer implements InstructionContainer {
	
	private final String prefix;
	private final String uri;
	
	public AbstractContainer(final String prefix, final String uri) {
		this.prefix = prefix;
		this.uri = uri;
	}
	
	public String name() {
		return prefix;
	}
	
	public String namespace() {
		return uri;
	}
	
	public final Map<String, Instruction> map = 
		new FastMap<String, Instruction>();
	
	protected void addInstruction(Instruction instruction) {
		map.put(instruction.name(), instruction);
	}

	public Instruction getInstruction(String name) {
		return map.get(name);
	}
}
