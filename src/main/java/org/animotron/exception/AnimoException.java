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
package org.animotron.exception;

import javolution.util.FastTable;

import org.neo4j.graphdb.Relationship;

import java.util.List;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class AnimoException extends Exception {
	
	private static final long serialVersionUID = 2923211415661487122L;

	private Relationship op;
	private List<Relationship> stack = new FastTable<Relationship>();

	public AnimoException(Relationship op) {
		super();
		this.op = op;
	}

	public AnimoException(Relationship op, Throwable cause) {
		super(cause);
		this.op = op;
	}

	public AnimoException(Relationship op, String message) {
		super(message);
		this.op = op;
	}

	public AnimoException(Exception e) {
		super(e);
	}

	public void addToStack(Relationship op) {
		stack.add(op);
	}
	
	public String toString() {
		return super.toString()+" @"+op;
	}

}
