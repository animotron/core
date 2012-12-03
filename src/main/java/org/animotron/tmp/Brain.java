/*
 *  Copyright (C) 2012 The Animo Project
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
package org.animotron.tmp;

import javolution.util.FastList;
import javolution.util.FastSet;
import org.animotron.statement.operator.*;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

import static org.neo4j.graphdb.traversal.Evaluation.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Brain {

	FastSet<Relationship> sensorState = new FastSet<Relationship>();
	FastSet<Relationship> detectedSensorObjects = new FastSet<Relationship>();
	
	FastSet<MentalObject> mentalState = new FastSet<MentalObject>();
	
	public Brain() {
		sensorState = FastSet.newInstance();
		mentalState = FastSet.newInstance();
	}
	
	protected void replaceSensorState(FastSet<Relationship> state) {
		FastSet.recycle(sensorState);
		sensorState = state;
		
		checkSensorStateForCompleteObjects();
	}
	
	protected void replaceMentalState(FastSet<MentalObject> state) {
		FastSet.recycle(mentalState);
		mentalState = state;
	}
	
	private void detectedSensorObject(Relationship r) {
		detectedSensorObjects.add(r);
		
		for (Path path : tdSensorToMental.traverse(r.getStartNode())) {
//			System.out.println(fs);
			
			boolean matched = false;
			Relationship last = path.lastRelationship();
			for (MentalObject m : mentalState) {
				if (m.steps.getFirst().getStartNode().equals(last.getEndNode())) {
//					System.out.println("match by first "+m.steps.getFirst());
					m.steps.addFirst(last);
					matched = true;
					continue;
				}
				if (m.steps.getLast().getEndNode().equals(last.getStartNode())) {
//					System.out.println("match by last "+m.steps.getLast());
					m.steps.addLast(last);
					matched = true;
					continue;
				}
			}
			if (!matched)
				mentalState.add(new MentalObject(last));
		}
	}
	
	private void checkSensorStateForCompleteObjects() {
		for (Relationship r : sensorState) {
	        Relationship ar = r.getStartNode().getSingleRelationship(ASHIFT._, Direction.INCOMING);
	        if (ar != null)
	        	detectedSensorObject(r);
		}
	}

	private static void sensorStep(Node n, Brain brain) {
		FastSet<Relationship> state = FastSet.newInstance();
		
		for (Path path : td.traverse(n)) {
			
			Relationship r = path.lastRelationship();
			Node end = r.getEndNode();
			
			if (!Utils.haveContext(end)) {
				state.add(r);
			} else
				for (Relationship rr : brain.sensorState) {
					if (rr.getStartNode().equals(r.getEndNode())) {
						state.add(r);
						break;
					}
				}
//			brain.detectedSensorObject(r);
			
			//check next levels
//			mentalStep(r.getStartNode(), brain);
		}
		brain.replaceSensorState(state);
	}
	
	public static Brain parse(String sentence) {
		Node n = null;

		Brain brain = new Brain();

		for (int i = 0; i < sentence.length(); i++) {
			n = VALUE._.get(sentence.charAt(i));
			if (n == null) {
				System.out.println("noise point '"+sentence.charAt(i)+"'");
				
			} else
				sensorStep(n, brain);
		}
			
		System.out.println(brain.toString());

		return brain;
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		
		sb.append("detected sensor objects: [ ");
		for (Relationship r : detectedSensorObjects) {
			sb.append(r.getId()).append(" ");
		}
		sb.append("]\n");

		sb.append("sensor state: [ ");
		for (Relationship r : sensorState) {
			sb.append(r.getId()).append(" ");
		}
		sb.append("]\n");

		sb.append("mental state: [ ");
		for (MentalObject mo : mentalState) {
			sb.append(mo.toString()).append(" ");
		}
		sb.append("]");
		
		return sb.toString();
	}
	
	class MentalObject {
		FastList<Relationship> steps = new FastList<Relationship>();
		
		public MentalObject(Relationship r) {
			steps.add(r);
		}
		
		public String toString() {
			StringBuilder sb = new StringBuilder();
			for (Relationship r : steps) {
				for (Relationship rr : r.getEndNode().getRelationships(REF._, Direction.OUTGOING)) {
					sb.append(" ").append(DEF._.reference(rr));
				}
				sb.append(" [").append(r.getId()).append("]");
			}
			return sb.toString();
		}
	}
	
	protected static TraversalDescription td = Traversal.description().
		depthFirst().
		uniqueness(Uniqueness.RELATIONSHIP_GLOBAL).
        evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
			@Override
			public Evaluation evaluate(Path path) {

				if (path.length() == 0)
					return EXCLUDE_AND_CONTINUE;
				
				Relationship r = path.lastRelationship();
				if (!r.getStartNode().equals(path.endNode()))
					return EXCLUDE_AND_PRUNE;
				
				if (path.length() == 1)
					if (r.isType(VALUE._))
						return EXCLUDE_AND_CONTINUE;

				if (path.length() == 2)
					if (r.isType(ASHIFT._))
						return EXCLUDE_AND_CONTINUE;

				if (path.length() == 3)
					if (r.isType(REF._))
						return EXCLUDE_AND_CONTINUE;

				if (path.length() == 4)
					if (r.isType(AN._))
						return INCLUDE_AND_PRUNE;

				return EXCLUDE_AND_PRUNE;
			}
        });
	
	protected static TraversalDescription tdSensorToMental = Traversal.description().
		depthFirst().
		uniqueness(Uniqueness.RELATIONSHIP_GLOBAL).
        evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
			@Override
			public Evaluation evaluate(Path path) {

				if (path.length() == 0)
					return EXCLUDE_AND_CONTINUE;
				
				Relationship r = path.lastRelationship();
				if (!r.getStartNode().equals(path.endNode()))
					return EXCLUDE_AND_PRUNE;
				
				if (path.length() == 1)
					if (r.isType(ASHIFT._))
						return EXCLUDE_AND_CONTINUE;

				if (path.length() == 2)
					if (r.isType(REF._))
						return EXCLUDE_AND_CONTINUE;

				if (path.length() == 3)
					if (r.isType(AN._))
						return EXCLUDE_AND_CONTINUE;

				if (path.length() == 4)
					if (r.isType(VALUE._))
						return EXCLUDE_AND_CONTINUE;

				if (path.length() == 5)
					if (r.isType(ASHIFT._))
						return EXCLUDE_AND_CONTINUE;

				if (path.length() == 6)
					if (r.isType(REF._))
						return EXCLUDE_AND_CONTINUE;

				if (path.length() == 7)
					if (r.isType(AN._))
						return INCLUDE_AND_PRUNE;

				return EXCLUDE_AND_PRUNE;
			}
        });

	protected static TraversalDescription tdMental = Traversal.description().
		depthFirst().
		uniqueness(Uniqueness.RELATIONSHIP_GLOBAL).
        evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
			@Override
			public Evaluation evaluate(Path path) {

				if (path.length() == 0)
					return EXCLUDE_AND_CONTINUE;
				
				Relationship r = path.lastRelationship();
				if (!r.getStartNode().equals(path.endNode()))
					return EXCLUDE_AND_PRUNE;
				
				if (path.length() == 1)
					if (r.isType(ASHIFT._))
						return EXCLUDE_AND_CONTINUE;

				if (path.length() == 2)
					if (r.isType(REF._))
						return EXCLUDE_AND_CONTINUE;

				if (path.length() == 3)
					if (r.isType(AN._))
						return INCLUDE_AND_PRUNE;

				return EXCLUDE_AND_PRUNE;
			}
        });
}