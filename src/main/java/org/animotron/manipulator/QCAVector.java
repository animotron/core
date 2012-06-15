/*
 *  Copyright (C) 2011-2012 The Animo Project
 *  http://animotron.org
 *  	
 *  This file is part of Animotron.
 *  
 *  Animotron is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as 
 *  published by the Free Software Foundation, either version 3 of 
 *  the License, or (at your option) any later version.
 *  
 *  Animotron is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *  
 *  You should have received a copy of 
 *  the GNU Affero General Public License along with Animotron.  
 *  If not, see <http://www.gnu.org/licenses/>.
 */
package org.animotron.manipulator;

import javolution.util.FastSet;
import javolution.util.FastTable;
import org.animotron.exception.AnimoException;
import org.animotron.statement.operator.AREV;
import org.animotron.statement.operator.DEF;
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.Utils;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.DataOutputStream;
import java.io.IOException;
import java.util.List;

import static org.animotron.utils.MessageDigester.longToByteArray;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class QCAVector {
	
//	private final Path qPath = null;
//	private final Path aPath = null;
//	private final List<Path> aPaths = null;
	
	private final Relationship question;
	private Relationship answer = null;
	
	private List<QCAVector> answers = null;

	private List<QCAVector> context = null;

//	private QCAVector preceding_sibling = null;
	
	private static boolean debug = false;

	public QCAVector(Relationship question) {
		this.question = question;
//		if (question != null && question.isType(DEF._))
//			answer = question;
		
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
		
//		this.preceding_sibling = preceding_sibling;
//		if (debug) 
//		if (preceding_sibling == this)
//			System.out.println(" .... create vector 7 .... "+this);
	}

	public QCAVector(Relationship question, Relationship context, Relationship answer) {
		this.question = question;
		this.answer = answer;

		this.context = new FastTable<QCAVector>();
		this.context.add(new QCAVector(null, answer));
		if (debug) System.out.println(" .... create vector 8 .... ");
	}

	public QCAVector(Relationship question, QCAVector context, QCAVector precedingSibling) throws AnimoException {
		this.question = question;
		//this.answer = answer;
		
		this.context = new FastTable<QCAVector>();
		this.context.add(context);
		
//		this.preceding_sibling = precedingSibling;
//		if (debug) 
//		if (preceding_sibling == this)
//			System.out.println(" .... create vector 9 .... "+this);
		
		cyclingDetection(question);
	}
	
	protected void cyclingDetection(Relationship op) throws AnimoException {
		if (op.isType(REF._)) return;
		
		if (context != null)
			for (QCAVector v : context)
				if (v.hasRelationship(op))
					throw new AnimoException(op, "cycling detected for "+op.getId()+" at "+this);

		
//		if (preceding_sibling != null)
//			preceding_sibling.cyclingDetection(op);
	}

	public Relationship getClosest() {
		if (answer != null)	return getAnswer(); 
		
		return question;
	}

	public Node getClosestEndNode() {
		Relationship r = getClosest();
		
		Node n = r.getEndNode();
		try {
			return AREV._.actualNode(n);
		} catch (Exception e) {
			return n;
		}
	}

	public Node getAnswerEndNode() {
		Relationship r = getUnrelaxedAnswer();
		
		Node n = r.getEndNode();
		try {
			return AREV._.actualNode(n);
		} catch (Exception e) {
			return n;
		}
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
	
//	public void setPrecedingSibling(QCAVector prev) {
//		if (preceding_sibling == this) {
//			System.out.println("setPrecedingSibling "+preceding_sibling);
//			System.out.println(prev);
//		}
//		preceding_sibling = prev;
//	}
//
//	public QCAVector getPrecedingSibling() {
//		if (preceding_sibling == this) {
//			System.out.println("getPrecedingSibling "+preceding_sibling);
//			System.out.println(this);
//		}
//		return preceding_sibling;
//	}

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
	
	private void spaces(StringBuilder b, int n) {
		for (int i = 0; i < n; i++)
			b.append(" ");
	}
	
	public void debug(StringBuilder b, FastSet<Relationship> visited, int level) {
		//spaces(b, level);
		b.append(" ");
		if (question == null) {
			b.append("NULL");
		} else {
			if (visited.contains(question)) {
				spaces(b, level);
				b.append(" cycling detected "+question);
				return;
			}
			visited.add(question);
				
			b.append(" '");
			b.append(question.getType());
			b.append("'");
			
			for (Relationship r : question.getEndNode().getRelationships(REF._, Direction.OUTGOING)) {
				b.append(" "+ DEF._.reference(r));
			}
			b.append("[");
			b.append(question.getId());
			b.append("]");
			
//			try {
//				Pipe thes = AN.getREFs(null, new QCAVector(question));
//				QCAVector v;
//				while ((v = thes.take()) != null) {
//					Object name = DEF._.reference(v.getClosest());
//					if (name != null) {
//						b.append(" ");
//						b.append(name);
//					}
//				}
//			} catch (Throwable t) {
//			}
		}
		if (context != null) {
			b.append(" {\n");
			int i = 0;
			for (QCAVector c : context) {
				spaces(b, level);
				b.append(i); i++;
				b.append("=");
				
				c.debug(b, visited, level+1);
			}
			b.append("}\n");
		} else {
			b.append(" {}\n");
		}
		spaces(b, level);
		if (answer != null) {
			if (visited.contains(answer)) {
				b.append(" cycling detected "+answer);
				return;
			}

			b.append("   ");
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
				
				a.debug(b, visited, level+1);
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
		b.append("]\n");

		if (debug) System.out.println("DEBUG START");
		debug(b, visited, 0);
		if (debug) System.out.println("DEBUG END");
		return b.toString();
	}

	public boolean hasRelationship(Relationship r) {
		
		if (r == null)
			return false;

		boolean debug = false;
		
		long id = r.getId();
		
		if (debug) System.out.print("hasRelationship "+question+" ("+question.getType()+") ");
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
				if (c.hasRelationship(r))
					return true;
			}
		}
		
//		if (preceding_sibling != null)
//			return preceding_sibling.hasRelationship(r);
		
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
//		if (preceding_sibling == this)
//			System.out.println("!!!answered 1 "+this);
//
//		return new QCAVector(question, createdAnswer, context, preceding_sibling);
		return new QCAVector(question, createdAnswer, context);
	}

	public QCAVector answered(Relationship createdAnswer, QCAVector context) {
		//context.removeThis(this);
//		if (preceding_sibling == this)
//			System.out.println("!!!answered 2 "+this);
		
		FastTable<QCAVector> c = new FastTable<QCAVector>();
		c.add(context);
		if (this.context != null) {
			for (QCAVector v : this.context) {
				if (v == context) continue;
				c.add(v);
			}
		}
			
//		return new QCAVector(question, createdAnswer, c, preceding_sibling);
		return new QCAVector(question, createdAnswer, c);
	}
	
	public QCAVector answered(Relationship createdAnswer, List<QCAVector> contexts) {
//		if (preceding_sibling == this)
//			System.out.println("!!!answered 3 "+this);
//
//		return new QCAVector(question, createdAnswer, contexts, preceding_sibling);
		return new QCAVector(question, createdAnswer, contexts);
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
			//v.setPrecedingSibling(this);
			return v;
		}
		
		return new QCAVector(q, this);
	}
	
	public QCAVector question(Relationship q, QCAVector prev) throws AnimoException {
		if (question != null && question.equals(q) && answer == null)
//		if (question != null && question.equals(q) && preceding_sibling == prev && answer == null)
			return this;
		
		cyclingDetection(q);
		
		//System.out.println("question "+this);
		//System.out.println(prev);
		
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

	public boolean hasAnswer() {
		return (
			(answers != null) 
			|| (
					(getUnrelaxedAnswer() != null) && 
					(!getUnrelaxedAnswer().equals(getQuestion()))
			)
		);
	}

	public Node getClosestDefEndNode() {
		Node node = getClosestEndNode();
		try {
			return DEF._.getDef( node );
		} catch (Throwable e) {
		}
		return node;
	}
}
