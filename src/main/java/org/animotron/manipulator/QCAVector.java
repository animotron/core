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

import org.animotron.io.PipedInput;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.animotron.statement.operator.Utils;
import org.neo4j.graphdb.Relationship;

import java.io.DataOutputStream;
import java.io.IOException;

import static org.animotron.utils.MessageDigester.longToByteArray;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class QCAVector {
	
	private Relationship question = null;
	private Relationship answer = null;
	
	private QCAVector[] context = null;

	public QCAVector(Relationship question) {
		this.question = question;
	}

	public QCAVector(Relationship question, Relationship answer) {
		this.question = question;
		this.answer = answer;
	}

	public QCAVector(Relationship question, QCAVector context, Relationship answer) {
		this.question = question;
		if (context != null)
			this.context = new QCAVector[] {context};
		this.answer = answer;
	}
	
	public QCAVector(Relationship question, QCAVector ... context) {
		this.question = question;
		this.context = context;
	}

	public QCAVector(Relationship question, Relationship answer, QCAVector ... context) {
		this.question = question;
		this.context = context;
		this.answer = answer;
	}

	public QCAVector(Relationship question, Relationship context, Relationship answer) {
		this.question = question;
		this.context = new QCAVector[] {new QCAVector(null, answer)};
		this.answer = answer;
	}

	public Relationship getClosest() {
		if (answer != null)	return getAnswer(); 
		
		return question;
	}

	public Relationship getUnrelaxedClosest() {
		if (answer != null)	return answer; 
		
		return question;
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

	public QCAVector[] getContext() {
		return context;
	}

	public boolean questionEquals(QCAVector vector) {
		return question.equals(vector.question);
	}

	protected void collectHash(DataOutputStream dos) throws IOException {
		if (question	!= null) dos.writeLong(question.getId());
		if (answer		!= null) dos.writeLong(answer.getId());
		
		if (context		!= null) {
			for (QCAVector c : context) {
				c.collectHash(dos);
			}
		}
	}

	public byte[] mashup() {
		//XXX: what is this?
        return longToByteArray(answer.getId());
	}
	
	public void debug(StringBuilder b) {
		b.append("QCA(");
		if (question == null)
			b.append("NULL");
		else {
			b.append(question.getId());
			b.append(" '");
			b.append(question.getType());
			b.append("'");
			
			try {
				PipedInput<QCAVector> thes = AN.getREFs(null, question);
				for (QCAVector v : thes) {
					Object name = THE._.reference(v.getClosest());
					if (name != null) {
						b.append(" ");
						b.append(name);
					}
				}
			} catch (Exception e) {
			}
		}
		b.append(" {");
		if (context != null) {
			int i = 0;
			for (QCAVector c : context) {
				b.append(i); i++;
				b.append("=");
				
				c.debug(b);
			}
		}
		b.append("}");
		if (answer != null) {
			b.append(" ");
			b.append(answer.getId());
			b.append(" '");
			b.append(answer.getType());
			b.append("'");
		}
		b.append(")");
	}

	public String toString() {
		StringBuilder b = new StringBuilder();
		debug(b);
		return b.toString();
	}

	public boolean haveRelationship(Relationship r) {

		boolean debug = false;
		
		long id = r.getId();
		
		if (debug) System.out.print("haveRelationship "+question+" ("+question.getType()+") ");
		if (question != null && question.getId() == id) return true;

		if (debug && answer != null) {
			System.out.print("answers "+answer+" ("+answer.getType()+") ");
			Relationship a = getAnswer();
			System.out.print(a+" ("+a.getType()+") ");
		}
		if (debug) System.out.println();
		
		if (answer != null && (answer.getId() == id || getAnswer().getId() == id)) return true;
		
		if (context != null) {
			for (QCAVector c : context) {
				if (c.haveRelationship(r))
					return true;
			}
		}
		
		return false;
	}

	public boolean canBeMerged(QCAVector vector) {
		if (question == null 
			|| vector.question == null 
			|| answer != null 
			|| context != null 
			) 
			return false;
		
		if (question.getId() != vector.question.getId()) return false;
		
		return true;
	}
	
	public int hashCode() {
		int hash = 0;
		if (question != null) hash += question.getId();
		if (answer != null) hash += answer.getId();
		
		return hash;
	}
	
	public boolean equals(Object o) {
		if (o == null || !(o instanceof QCAVector)) {
			return false;
		}
		QCAVector oV = (QCAVector) o;
		
		//XXX:check context too?
		if ((question == null && oV.question == null) 
				|| (question != null && oV.question != null && question.equals(oV.question)))
				if ((answer == null && oV.answer == null) 
						|| (answer != null && oV.answer != null && answer.equals(oV.answer)))
					return true;
		
		return false;
	}
}
