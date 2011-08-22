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
package org.animotron.graph;

import static org.animotron.graph.RelationshipTypes.REF;

import java.util.List;

import javolution.util.FastList;

import org.animotron.Statement;
import org.animotron.operator.AN;
import org.animotron.operator.query.ALL;
import org.animotron.operator.query.ANY;
import org.animotron.operator.relation.HAVE;
import org.animotron.operator.relation.IS;
import org.animotron.operator.relation.USE;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class AnimoPath {

	Path path;
	
	List<Pattern> patterns = new FastList<Pattern>();
	
	List<Step> parsed = new FastList<Step>(); 
	
	public AnimoPath(Path path) {
		this.path = path;
		
		patterns.add(
			new Pattern(
				USE._, 
				many(IS._), one(USE._)
			)
		);
		patterns.add(
			new Pattern(
				ANY._, 
				many(IS._), one(REF), one(ANY._)
			)
		);
		patterns.add(
			new Pattern(
				AN._, 
				one(REF), one(AN._)
			)
		);
		patterns.add(
			new Pattern(
				HAVE._, 
				one(REF), one(HAVE._)
			)
		);

		parse();
	}
	
	private void parse() {
		int pos = 0;
		
		List<Step> steps = new FastList<Step>();
		for (Pattern pattern : patterns) {
			steps.add(new Step(pattern));
		}

		boolean matched = false;
		for (Relationship r : path.relationships()) {
			
			matched = false;
			for (Step step : steps) {
				if (step.match(r)) { 
					if (step.done()) {
						parsed.add(new Step(step));
						step.clear();
						matched = true;
					}
				} else {
					step.clear();
				}
			}
			
			if (!matched) {
				if (r.getType().name().equals(AN._.rType))
					parsed.add(new Step(new Pattern(AN._)));
				if (r.getType().name().equals(ANY._.rType))
					parsed.add(new Step(new Pattern(ANY._)));
				if (r.getType().name().equals(ALL._.rType))
					parsed.add(new Step(new Pattern(ALL._)));
			}
				
			pos++;
		}
	}
	
	public String toString() {
		String str = "";
		for (Step step : parsed) {
			str += step.toString() + " < ";
		}
		return str;
	}
	
	class Pattern {
		
		Cardinality[] patternSteps;
		Statement st;
		
		public Pattern(Statement st, Cardinality... patternStep) {
			this.st = st;
			patternSteps = patternStep;
		}
	}
	
	private One one(Statement st) {
		return new One(st.relationshipType());
	}

	private One one(RelationshipType rt) {
		return new One(rt);
	}

	private Many many(Statement st) {
		return new Many(st.relationshipType());
	}

	private Many many(RelationshipType rt) {
		return new Many(rt);
	}

	abstract class Cardinality {
		RelationshipType type;

		public Cardinality(RelationshipType type) {
			this.type = type;
		}
		
		public RelationshipType getType() {
			return type;
		}
	}
	
	class One extends Cardinality {
		public One(RelationshipType type) {
			super(type);
		}
	}

	class Many extends Cardinality {
		public Many(RelationshipType type) {
			super(type);
		}
	}
	
	class Step {
		
		Pattern pattern;
		List<Relationship> path = new FastList<Relationship>();
		
		int pos = 0;
		
		Boolean matched = null;
		
		public Step(Pattern pattern) {
			this.pattern = pattern;
		}

		public Step(Step step) {
			pattern = step.pattern;
			path = step.path;
		}

		public void clear() {
			pos = 0;
			matched = null;
		}

		public boolean done() {
			return pattern.patternSteps.length == pos;
		}

		public boolean match(Relationship r) {
			 if (done()) return false;
			
			Cardinality cardinality = pattern.patternSteps[pos];

			if (cardinality.getType().name().equals(r.getType().name())) {
				if (cardinality instanceof One) {
					pos++;
					matched = null;
				} else {
					matched = true;
				}
				path.add(r);
				return true;
			
			} else if (matched == null) {
				return false;
			
			}
			
			pos++;
			matched = null;
			
			return match(r);
		}
		
		public String toString() {
			return pattern.st.name();
		}
	}
}