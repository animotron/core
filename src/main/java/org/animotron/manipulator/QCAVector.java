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

import org.animotron.exception.AnimoException;
import org.animotron.io.PipedInput;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.animotron.statement.operator.Utils;
import org.neo4j.graphdb.Relationship;

import java.io.DataOutputStream;
import java.io.IOException;
import java.util.List;

import javolution.util.FastSet;
import javolution.util.FastTable;

import static org.animotron.utils.MessageDigester.longToByteArray;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class QCAVector {
	
	private final Relationship question;
	private Relationship answer = null;
	
	private List<QCAVector> answers = null;

	private List<QCAVector> context = null;

	private QCAVector preceding_sibling = null;
	
	private boolean debug = false;

	public QCAVector(Relationship question) {
		this.question = question;
		if (debug) System.out.println(" .... create vector 1 .... ");
	}

	public QCAVector(Relationship question, Relationship answer) {
		this.question = question;
		this.answer = answer;
		if (debug) System.out.println(" .... create vector 2 .... ");
	}

	public QCAVector(Relationship question, QCAVector context, Relationship answer) {
		this.question = question;
		if (context != null) {
			this.context = new FastTable<QCAVector>();
			this.context.add(context);
		}
		this.answer = answer;
		if (debug) System.out.println(" .... create vector 3 .... ");
	}
	
	public QCAVector(Relationship question, QCAVector context) {
		this.question = question;

		this.context = new FastTable<QCAVector>();
		this.context.add(context);
		if (debug) System.out.println(" .... create vector 4 .... ");
	}
	
	public QCAVector(Relationship question, List<QCAVector> context) {
		this.question = question;

		this.context = context;
		if (debug) System.out.println(" .... create vector 4 .... ");
	}

	public QCAVector(Relationship question, Relationship answer, QCAVector context) {
		this.question = question;
		this.answer = answer;

		this.context = new FastTable<QCAVector>();
		this.context.add(context);
		if (debug) System.out.println(" .... create vector 5 .... ");
	}

	public QCAVector(Relationship question, Relationship answer, List<QCAVector> context) {
		this.question = question;
		this.answer = answer;
		
		this.context = context;
		if (debug) System.out.println(" .... create vector 6 .... ");
	}

	public QCAVector(Relationship question, Relationship answer, List<QCAVector> context, QCAVector preceding_sibling) {
		this.question = question;
		this.answer = answer;
		
		this.context = context;
		
		this.preceding_sibling = preceding_sibling;
		if (debug) System.out.println(" .... create vector 7 .... ");
	}

	public QCAVector(Relationship question, Relationship context, Relationship answer) {
		this.question = question;
		this.answer = answer;

		this.context = new FastTable<QCAVector>();
		this.context.add(new QCAVector(null, answer));
		if (debug) System.out.println(" .... create vector 8 .... ");
	}

	public QCAVector(Relationship question, QCAVector context, QCAVector precedingSibling) {
		this.question = question;
		//this.answer = answer;
		
		this.context = new FastTable<QCAVector>();
		this.context.add(context);
		
		this.preceding_sibling = precedingSibling;
		if (debug) System.out.println(" .... create vector 9 .... ");
	}
	
	protected void cyclingDetection(Relationship op) throws AnimoException {
		int deep = 0; int count = 0;
		if (context != null)
			for (QCAVector v : context) {
				if (deep > 0 && v.haveRelationship(op)) {
					if (count > 2)
						throw new AnimoException(op, "cycling detected "+this);
		            else
						count++;
				}
				deep++;
			}
		
		if (preceding_sibling != null)
			preceding_sibling.cyclingDetection(op);
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

	public void setAnswer(Relationship r) {
		answer = r;
	}

	public Relationship getAnswer() {
		return Utils.relax(answer);
	}
	
	public Relationship getUnrelaxedAnswer() {
		return answer;
	}

	public void addContext(List<QCAVector> cs) {
		if (context == null)
			context = cs;
		else
			context.addAll(cs);
	}

	public List<QCAVector> getContext() {
		return context;
	}
	
	public void setPrecedingSibling(QCAVector prev) {
		preceding_sibling = prev;
	}

	public QCAVector getPrecedingSibling() {
		return preceding_sibling;
	}

	public boolean questionEquals(QCAVector vector) {
		return question.equals(vector.question);
	}

	protected void collectHash(DataOutputStream dos) throws IOException {
		if (question	!= null) dos.writeLong(question.getId());
		if (answer		!= null) dos.writeLong(answer.getId());
		
		//System.out.println(" hash "+super.hashCode());
		if (context		!= null) {
			for (QCAVector c : context) {
				//System.out.println(" - hash "+super.hashCode());
				c.collectHash(dos);
			}
		}
	}

	public byte[] mashup() {
		//XXX: what is this?
        return longToByteArray(answer.getId());
	}
	
	public void debug(StringBuilder b, FastSet<Relationship> visited) {
		b.append("QCA(");
		if (question == null)
			b.append("NULL");
		else {
			if (visited.contains(question)) {
				b.append(" cycling detected "+question);
				return;
			}
				
			b.append(question.getId());
			b.append(" '");
			b.append(question.getType());
			b.append("'");
			
			try {
				PipedInput<QCAVector> thes = AN.getREFs(null, new QCAVector(question));
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
				
				c.debug(b, visited);
			}
		}
		b.append("}");
		if (answer != null) {
			if (visited.contains(answer)) {
				b.append(" cycling detected "+answer);
				return;
			}

			b.append(" ");
			b.append(answer.getId());
			b.append(" '");
			b.append(answer.getType());
			b.append("'");
		}
		
		if (answers != null) {
			b.append(" *");
			boolean first = true;
			for (QCAVector a : answers) {
				if (!first)
					b.append(", ");
				else
					first = false;
				
				a.debug(b, visited);
			}
			b.append("*");
			
		}
		b.append(")");
	}

	public String toString() {
		StringBuilder b = new StringBuilder();
		
		FastSet<Relationship> visited = new FastSet<Relationship>();
		
		b.append("[");
		b.append(super.hashCode());
		b.append("] ");

		if (debug) System.out.println("DEBUG START");
		debug(b, visited);
		if (debug) System.out.println("DEBUG END");
		return b.toString();
	}

	public boolean haveRelationship(Relationship r) {
		
		if (r == null)
			return false;

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
		
		if (preceding_sibling != null)
			return preceding_sibling.haveRelationship(r);
		
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

	public boolean merged(QCAVector vector) {
		if (canBeMerged(vector)) {
			answer = vector.answer;
			context = vector.context;
			return true;
		}
		return false;
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

	public QCAVector answered(Relationship createdAnswer) {
		 return new QCAVector(question, createdAnswer, context, preceding_sibling);
	}

	public QCAVector answered(Relationship createdAnswer, QCAVector context) {
		//context.removeThis(this);
		
		FastTable<QCAVector> c = new FastTable<QCAVector>();
		c.add(context);
		if (this.context != null) {
			for (QCAVector v : this.context) {
				if (v == context) continue;
				c.add(v);
			}
		}
			
		return new QCAVector(question, createdAnswer, c, preceding_sibling);
	}
	
	public QCAVector answered(Relationship createdAnswer, List<QCAVector> contexts) {
		return new QCAVector(question, createdAnswer, contexts, preceding_sibling);
	}

	public QCAVector question(Relationship q) {
		if (question != null && question.equals(q) && answer == null)
			return this;
		
		return new QCAVector(q, this);
	}

	public QCAVector question2(Relationship q) {
		if (question != null && question.equals(q) && answer == null)
			return this;
		
		if (answer == null) {
			QCAVector v = new QCAVector(q, context);
			v.setPrecedingSibling(this);
			return v;
		}
		
		return new QCAVector(q, this);
	}
	
	public QCAVector question(Relationship q, QCAVector prev) throws AnimoException {
		if (question != null && question.equals(q) && preceding_sibling == prev && answer == null)
			return this;
		
		cyclingDetection(q);
		
		return new QCAVector(q, this, prev);
	}

	public void addAnswer(QCAVector i) {
		if (answers == null)
			answers = new FastTable<QCAVector>();
		
		answers.add(i);
	}

	public List<QCAVector> getAnswers() {
		return answers;
	}
	
	public void recycle() {
		if (context != null) {
			for (QCAVector c : context)
				c.recycle();
			
			if (context instanceof FastTable)
				FastTable.recycle((FastTable<QCAVector>) context);
		}
		
		if (answers != null) {
			for (QCAVector c : answers)
				c.recycle();

			if (answers instanceof FastTable)
				FastTable.recycle((FastTable<QCAVector>) answers);
		}
	}
}