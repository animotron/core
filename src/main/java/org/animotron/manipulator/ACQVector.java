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
package org.animotron.manipulator;

import java.io.DataOutputStream;
import java.io.IOException;

import org.animotron.statement.operator.Utils;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ACQVector {
	
	private Relationship question = null;
	private Relationship answer = null;
	
	private ACQVector context = null;

	public ACQVector(Relationship question) {
		this.question = question;
	}

	public ACQVector(Relationship question, Relationship answer) {
		this.question = question;
		this.answer = answer;
	}

	public ACQVector(Relationship question, ACQVector context, Relationship answer) {
		this.question = question;
		this.context = context;
		this.answer = answer;
	}
	
	public ACQVector(Relationship question, Relationship context, Relationship answer) {
		this.question = question;
		this.context = new ACQVector(null, answer);
		this.answer = answer;
	}

	public Relationship getQuestion() {
		return question;
	}

	public Relationship getAnswer() {
		return Utils.relax(answer);
	}
	
	public Relationship getUnrelaxedAnswer() {
		return answer;
	}
	

	public ACQVector getContext() {
		return context;
	}

	public boolean questionEquals(ACQVector vector) {
		return question.equals(vector.question);
	}

	protected void collectHash(DataOutputStream dos) throws IOException {
		dos.writeLong(question.getId());
		if (answer != null)
			dos.writeLong(answer.getId());
		if (context != null)
			context.collectHash(dos);
	}

	public byte[] mashup() {
		//XXX: what is this?
		//XXX: optimize!!!
		return String.valueOf(answer.getId()).getBytes();
	}
}
