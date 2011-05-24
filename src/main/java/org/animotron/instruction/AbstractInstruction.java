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

import org.animotron.Container;
import org.animotron.graph.AnimoRelationshipType;
import org.neo4j.graphdb.RelationshipType;

/**
 * Abstract instruction.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public abstract class AbstractInstruction implements Instruction {
	
	private final String name;
	private final String prefix;
	private final String uri;
	
	private RelationshipType relationshipType;
	
	public AbstractInstruction(final Container operator, final String name) {
		this.name = name;
		this.prefix = operator.name();
		this.uri = operator.namespace();
		this.relationshipType = 
			AnimoRelationshipType.get(this.prefix.toUpperCase() + ":" + this.name.toUpperCase());
	}
	
	public String name() {
		return name;
	}

	public String prefix() {
		return prefix;
	}

	public String namespace() {
		return uri;
	}

	public RelationshipType relationshipType() {
		return relationshipType;
	}
	
}
